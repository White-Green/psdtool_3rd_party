use std::array;
use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Read;

use image::imageops::{flip_horizontal_in_place, flip_vertical_in_place};
use image::{GenericImage, GenericImageView, Rgba, RgbaImage};
use regex::{Captures, Regex};
use ya_psd::layer_info::{LayerRecordFlags, LayerTreeNode};
use ya_psd::Psd;

use crate::pfv::{Pfv, PfvParseError};

pub use ya_psd;

mod pfv;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct LayerName(String);

impl LayerName {
    pub fn new<S: AsRef<str>>(s: S) -> Self {
        s.into()
    }
    pub fn with_index<S: AsRef<str>>(s: S, index: usize) -> Self {
        let regex = Regex::new(r#"[\x00-\x1f\x22\x25\x27\x2f\x5c\x7e\x7f]"#).unwrap();
        let layer_name = regex.replace(s.as_ref(), |capture: &Captures| {
            assert_eq!(capture.len(), 1);
            let match_str = capture.get(0).unwrap().as_str();
            assert_eq!(match_str.len(), 1);
            format!("%{:2x}", match_str.as_bytes()[0])
        });
        if index == 0 {
            LayerName(layer_name.into_owned())
        } else {
            LayerName(format!("{}\\{}", layer_name, index))
        }
    }
    pub fn into_string(self) -> String {
        self.0
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl<S: AsRef<str>> From<S> for LayerName {
    fn from(value: S) -> Self {
        Self::with_index(value, 0)
    }
}

impl From<LayerName> for String {
    fn from(LayerName(layer_name): LayerName) -> Self {
        urlencoding::decode(&layer_name).unwrap().to_string()
    }
}

#[derive(Debug, Clone)]
enum LayerNameTreeNode {
    Node { flat_index: usize, path_index: usize, children: HashMap<LayerName, Flip<LayerNameTreeNode>> },
    Leaf { flat_index: usize, path_index: usize },
}

impl LayerNameTreeNode {
    fn flat_index(&self) -> usize {
        match self {
            LayerNameTreeNode::Node { flat_index, .. } => *flat_index,
            LayerNameTreeNode::Leaf { flat_index, .. } => *flat_index,
        }
    }
    fn path_index(&self) -> usize {
        match self {
            LayerNameTreeNode::Node { path_index, .. } => *path_index,
            LayerNameTreeNode::Leaf { path_index, .. } => *path_index,
        }
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct Flip<T> {
    default: Option<T>,
    flip_x: Option<T>,
    flip_y: Option<T>,
    flip_xy: Option<T>,
}

impl<T> Flip<T> {
    fn get(&self, flip_x: bool, flip_y: bool) -> Option<&T> {
        match (flip_x, flip_y) {
            (false, false) => self.default.as_ref(),
            (true, false) => self.flip_x.as_ref(),
            (false, true) => self.flip_y.as_ref(),
            (true, true) => self.flip_xy.as_ref(),
        }
    }
}

impl<T> Default for Flip<T> {
    fn default() -> Self {
        Flip { default: None, flip_x: None, flip_y: None, flip_xy: None }
    }
}

#[derive(Debug)]
pub struct PsdToolController<'a> {
    psd: Psd<'a>,
    pfv: Pfv,
    layer_name_tree: HashMap<LayerName, Flip<LayerNameTreeNode>>,
    visible: Vec<bool>,
    flip_x: bool,
    flip_y: bool,
}

#[derive(Debug)]
pub struct PsdToolControllerRef<'a, 'b> {
    psd: &'b Psd<'a>,
    pfv: &'b Pfv,
    layer_name_tree: &'b HashMap<LayerName, Flip<LayerNameTreeNode>>,
    visible: Vec<bool>,
    flip_x: bool,
    flip_y: bool,
}

impl<'a> PsdToolController<'a> {
    pub fn new(psd: Psd<'a>) -> Self {
        Self::with_pfv(psd, Pfv::empty())
    }
    pub fn with_pfv_file(psd: Psd<'a>, pfv: impl Read) -> Result<Self, PfvParseError> {
        Ok(Self::with_pfv(psd, Pfv::from_pfv_file(pfv)?))
    }
    fn with_pfv(psd: Psd<'a>, pfv: Pfv) -> Self {
        fn create_layer_tree(layers: &[LayerTreeNode], visible: &mut Vec<bool>, seq: &mut usize) -> HashMap<LayerName, Flip<LayerNameTreeNode>> {
            fn to_string(name: &[u8]) -> String {
                encoding_rs::SHIFT_JIS.decode(name).0.into_owned()
            }
            fn layer_name(layer: &LayerTreeNode) -> String {
                match layer {
                    LayerTreeNode::Leaf(layer) => to_string(layer.layer_name()),
                    LayerTreeNode::Node { folder, .. } => to_string(folder.layer_name()),
                }
            }
            fn layer_flags(layer: &LayerTreeNode) -> LayerRecordFlags {
                match layer {
                    LayerTreeNode::Leaf(layer) => layer.flags(),
                    LayerTreeNode::Node { folder, .. } => folder.flags(),
                }
            }
            #[derive(Debug, Eq, PartialEq, Hash)]
            enum FlipFlags {
                X,
                Y,
                Xy,
            }
            fn flip_index(mut name: &str) -> (String, HashSet<FlipFlags>) {
                let mut result = HashSet::new();
                loop {
                    if let Some(stripped) = name.strip_suffix(":flipx") {
                        name = stripped;
                        result.insert(FlipFlags::X);
                        continue;
                    } else if let Some(stripped) = name.strip_suffix(":flipy") {
                        name = stripped;
                        result.insert(FlipFlags::Y);
                        continue;
                    } else if let Some(stripped) = name.strip_suffix(":flipxy") {
                        name = stripped;
                        result.insert(FlipFlags::Xy);
                        continue;
                    }
                    break (name.to_string(), result);
                }
            }
            let mut result = HashMap::new();
            let mut layer_name_conflict = HashMap::new();
            for (in_folder_index, layer) in layers.iter().enumerate().rev() {
                let name = layer_name(layer);
                let is_show = name.starts_with('!');
                let index = *layer_name_conflict.entry(name.clone()).and_modify(|i| *i += 1).or_default();
                let node = match layer {
                    LayerTreeNode::Leaf(_) => LayerNameTreeNode::Leaf { flat_index: *seq, path_index: in_folder_index },
                    LayerTreeNode::Node { children, .. } => {
                        let children = create_layer_tree(children, visible, seq);
                        LayerNameTreeNode::Node { flat_index: *seq, path_index: in_folder_index, children }
                    }
                };
                let (name, flip_flags) = flip_index(&name);
                let flip: &mut Flip<LayerNameTreeNode> = result.entry(LayerName::with_index(name, index)).or_default();
                if flip_flags.is_empty() {
                    flip.default.replace(node);
                } else {
                    if flip_flags.contains(&FlipFlags::X) {
                        flip.flip_x.replace(node.clone());
                    }
                    if flip_flags.contains(&FlipFlags::Y) {
                        flip.flip_y.replace(node.clone());
                    }
                    if flip_flags.contains(&FlipFlags::Xy) {
                        flip.flip_xy.replace(node);
                    }
                }
                if visible.len() <= *seq {
                    visible.resize(*seq + 1, false);
                }
                visible[*seq] = is_show || !layer_flags(layer).contains(LayerRecordFlags::VISIBLE);
                *seq += 1;
            }
            result
        }
        let mut visible = Vec::new();
        let layer_name_tree = create_layer_tree(psd.layer_information().layer_info(), &mut visible, &mut 0);
        PsdToolController { psd, pfv, layer_name_tree, visible, flip_x: false, flip_y: false }
    }
}

#[derive(Debug)]
pub enum SwitchLayerError {
    PathNotFound,
}

impl Display for SwitchLayerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl Error for SwitchLayerError {}

pub trait PsdTool<'psd> {
    fn pdf(&self) -> &Psd<'psd>;
    fn clone_ref<'b>(&'b self) -> PsdToolControllerRef<'psd, 'b>;
    fn layer_list(&self) -> Vec<Box<[LayerName]>>;
    fn favorite_list(&self) -> Box<[String]>;
    fn apply_favorite(&mut self, path: &str) -> Result<(), SwitchLayerError>;
    fn switch_layer_by_string_path(&mut self, path: &str) -> Result<(), SwitchLayerError> {
        let path = path.split('/').map(|item| LayerName(item.to_string())).collect::<Vec<_>>();
        self.switch_layer(&path)
    }
    fn switch_layer(&mut self, path: &[LayerName]) -> Result<(), SwitchLayerError>;
    fn draw(&self) -> image::RgbaImage;
    fn flip_x(&mut self);
    fn flip_y(&mut self);
    fn get_visible(&self) -> &[bool];
    fn is_flip_x(&self) -> bool;
    fn is_flip_y(&self) -> bool;
}

impl<'psd> PsdTool<'psd> for PsdToolController<'psd> {
    fn pdf(&self) -> &Psd<'psd> {
        &self.psd
    }

    fn clone_ref<'b>(&'b self) -> PsdToolControllerRef<'psd, 'b> {
        PsdToolControllerRef {
            psd: &self.psd,
            pfv: &self.pfv,
            layer_name_tree: &self.layer_name_tree,
            visible: self.visible.clone(),
            flip_x: self.flip_x,
            flip_y: self.flip_y,
        }
    }

    fn layer_list(&self) -> Vec<Box<[LayerName]>> {
        get_layer_list(&self.layer_name_tree, self.flip_x, self.flip_y)
    }

    fn favorite_list(&self) -> Box<[String]> {
        self.pfv.get_member_names()
    }

    fn apply_favorite(&mut self, path: &str) -> Result<(), SwitchLayerError> {
        let (filtered, favorite_items) = self.pfv.get(path).ok_or(SwitchLayerError::PathNotFound)?;
        if !filtered {
            clear_optional_layers(&self.layer_name_tree, &mut self.visible);
        }
        let favorite_items = favorite_items.to_vec();
        for layer in favorite_items {
            self.switch_layer_by_string_path(&layer)?;
        }
        Ok(())
    }

    fn switch_layer(&mut self, path: &[LayerName]) -> Result<(), SwitchLayerError> {
        switch_layer(&self.layer_name_tree, &mut self.visible, path, self.flip_x, self.flip_y)
    }

    fn draw(&self) -> RgbaImage {
        draw(&self.psd, &self.layer_name_tree, &self.visible, self.flip_x, self.flip_y)
    }

    fn flip_x(&mut self) {
        self.flip_x = !self.flip_x;
    }

    fn flip_y(&mut self) {
        self.flip_y = !self.flip_y;
    }

    fn get_visible(&self) -> &[bool] {
        &self.visible
    }

    fn is_flip_x(&self) -> bool {
        self.flip_x
    }

    fn is_flip_y(&self) -> bool {
        self.flip_y
    }
}

impl<'psd, 'a> PsdTool<'psd> for PsdToolControllerRef<'psd, 'a> {
    fn pdf(&self) -> &Psd<'psd> {
        self.psd
    }

    fn clone_ref<'b>(&'b self) -> PsdToolControllerRef<'psd, 'b> {
        PsdToolControllerRef {
            psd: self.psd,
            pfv: self.pfv,
            layer_name_tree: self.layer_name_tree,
            visible: self.visible.clone(),
            flip_x: self.flip_x,
            flip_y: self.flip_y,
        }
    }

    fn layer_list(&self) -> Vec<Box<[LayerName]>> {
        get_layer_list(self.layer_name_tree, self.flip_x, self.flip_y)
    }

    fn favorite_list(&self) -> Box<[String]> {
        self.pfv.get_member_names()
    }

    fn apply_favorite(&mut self, path: &str) -> Result<(), SwitchLayerError> {
        let (filtered, favorite_items) = self.pfv.get(path).ok_or(SwitchLayerError::PathNotFound)?;
        if !filtered {
            clear_optional_layers(self.layer_name_tree, &mut self.visible);
        }
        let favorite_items = favorite_items.to_vec();
        for layer in favorite_items {
            if let Err(e) = self.switch_layer_by_string_path(&layer) {
                match e {
                    SwitchLayerError::PathNotFound => {
                        eprintln!("pathNotFound:{:?}", layer);
                    }
                }
            }
        }
        Ok(())
    }

    fn switch_layer(&mut self, path: &[LayerName]) -> Result<(), SwitchLayerError> {
        switch_layer(self.layer_name_tree, &mut self.visible, path, self.flip_x, self.flip_y)
    }

    fn draw(&self) -> RgbaImage {
        draw(self.psd, self.layer_name_tree, &self.visible, self.flip_x, self.flip_y)
    }

    fn flip_x(&mut self) {
        self.flip_x = !self.flip_x;
    }

    fn flip_y(&mut self) {
        self.flip_y = !self.flip_y;
    }

    fn get_visible(&self) -> &[bool] {
        &self.visible
    }

    fn is_flip_x(&self) -> bool {
        self.flip_x
    }

    fn is_flip_y(&self) -> bool {
        self.flip_y
    }
}

fn get_layer_list(name_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, flip_x: bool, flip_y: bool) -> Vec<Box<[LayerName]>> {
    let mut result = Vec::new();
    fn get_layer_list_inner(name_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, flip_x: bool, flip_y: bool, path: &mut Vec<LayerName>, result: &mut Vec<(Box<[LayerName]>, usize)>) {
        for (name, flip) in name_tree {
            path.push(name.clone());
            match flip.get(flip_x, flip_y) {
                Some(LayerNameTreeNode::Node { children, .. }) => {
                    get_layer_list_inner(children, flip_x, flip_y, path, result);
                }
                Some(LayerNameTreeNode::Leaf { flat_index, .. }) => {
                    result.push((path.clone().into_boxed_slice(), *flat_index));
                }
                _ => {}
            }
            path.pop().unwrap();
        }
    }
    get_layer_list_inner(name_tree, flip_x, flip_y, &mut Vec::new(), &mut result);
    result.sort_by_key(|(_, index)| *index);
    result.into_iter().map(|(path, _)| path).collect()
}

fn clear_optional_layers(name_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, visible: &mut [bool]) {
    for (name, Flip { default, flip_x, flip_y, flip_xy }) in name_tree {
        if !name.0.starts_with('!') && !name.0.starts_with('*') {
            if let Some(node) = default {
                visible[node.flat_index()] = false;
            }
            if let Some(node) = flip_x {
                visible[node.flat_index()] = false;
            }
            if let Some(node) = flip_y {
                visible[node.flat_index()] = false;
            }
            if let Some(node) = flip_xy {
                visible[node.flat_index()] = false;
            }
        }
        if let Some(LayerNameTreeNode::Node { children, .. }) = default {
            clear_optional_layers(children, visible);
        }
        if let Some(LayerNameTreeNode::Node { children, .. }) = flip_x {
            clear_optional_layers(children, visible);
        }
        if let Some(LayerNameTreeNode::Node { children, .. }) = flip_y {
            clear_optional_layers(children, visible);
        }
        if let Some(LayerNameTreeNode::Node { children, .. }) = flip_xy {
            clear_optional_layers(children, visible);
        }
    }
}

fn switch_layer(name_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, visible: &mut [bool], path: &[LayerName], flip_x: bool, flip_y: bool) -> Result<(), SwitchLayerError> {
    match path {
        [] => Err(SwitchLayerError::PathNotFound),
        [current] => {
            let current_node = name_tree.get(current).ok_or(SwitchLayerError::PathNotFound)?;
            let current_node = current_node.default.as_ref().unwrap();
            let current_node_index = current_node.flat_index();
            if current.as_str().starts_with('*') {
                for (name, node) in name_tree.iter() {
                    if name.as_str().starts_with('*') {
                        visible[node.get(flip_x, flip_y).unwrap_or_else(|| node.default.as_ref().unwrap()).flat_index()] = false;
                    }
                }
                visible[current_node_index] = true;
            } else if current.as_str().starts_with('!') {
                visible[current_node_index] = true;
            } else {
                visible[current_node_index] = !visible[current_node_index];
            }
            Ok(())
        }
        [current, path @ ..] => {
            let current_node = name_tree.get(current).ok_or(SwitchLayerError::PathNotFound)?;
            match current_node.get(flip_x, flip_y).unwrap_or_else(|| current_node.default.as_ref().unwrap()) {
                LayerNameTreeNode::Node { children, .. } => {
                    switch_layer(children, visible, path, flip_x, flip_y)?;
                    if current.as_str().starts_with('*') {
                        for (name, node) in name_tree.iter() {
                            if name.as_str().starts_with('*') {
                                visible[node.default.as_ref().unwrap().flat_index()] = false;
                            }
                        }
                    }
                    visible[current_node.default.as_ref().unwrap().flat_index()] = true;
                    Ok(())
                }
                LayerNameTreeNode::Leaf { .. } => Err(SwitchLayerError::PathNotFound),
            }
        }
    }
}

fn draw(psd: &Psd, layer_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, visible: &[bool], flip_x: bool, flip_y: bool) -> image::RgbaImage {
    let mut image: RgbaImage = RgbaImage::from_vec(psd.header().width(), psd.header().height(), array::IntoIter::new([[0, 0, 0, 0]]).cycle().take(psd.header().width() as usize * psd.header().height() as usize).flatten().collect()).unwrap();

    fn draw_inner(list: &[LayerTreeNode], layer_tree: &HashMap<LayerName, Flip<LayerNameTreeNode>>, visible: &[bool], image: &mut RgbaImage, flip_x: bool, flip_y: bool) {
        let mut visible_layers = layer_tree.values().filter(|node| visible[node.default.as_ref().unwrap().flat_index()]).collect::<Vec<_>>();
        visible_layers.sort_by_key(|node| Reverse(node.get(flip_x, flip_y).unwrap_or_else(|| node.default.as_ref().unwrap()).path_index()));
        for name_node in visible_layers {
            let name_node = name_node.get(flip_x, flip_y).unwrap_or_else(|| name_node.default.as_ref().unwrap());
            let node = &list[name_node.path_index()];
            let (new_image, blend_mode, left, top) = match node {
                LayerTreeNode::Leaf(layer) => {
                    let [r, g, b]: &[_; 3] = layer.channel_info().try_into().unwrap();
                    let a = layer.transparency_mask().unwrap();
                    let bytes = r.raw_data().iter().zip(g.raw_data()).zip(b.raw_data()).zip(a.raw_data()).flat_map(|(((&r, &g), &b), &a)| [r, g, b, a]).collect::<Vec<_>>();
                    if bytes.is_empty() {
                        continue;
                    }
                    let new_image: RgbaImage = RgbaImage::from_vec((layer.layer_right() - layer.layer_left()) as u32, (layer.layer_bottom() - layer.layer_top()) as u32, bytes).unwrap();
                    (new_image, layer.blend_mode(), layer.layer_left(), layer.layer_top())
                }
                LayerTreeNode::Node { folder, children } => {
                    let child_layer_tree = if let LayerNameTreeNode::Node { children, .. } = name_node { children } else { unreachable!() };

                    if folder.blend_mode() == ya_psd::layer_info::BlendMode::Passthrough {
                        draw_inner(children, child_layer_tree, visible, image, flip_x, flip_y);
                        continue;
                    } else {
                        let mut new_image = RgbaImage::from_pixel(image.width(), image.height(), Rgba::from([0; 4]));
                        draw_inner(children, child_layer_tree, visible, &mut new_image, flip_x, flip_y);
                        (new_image, folder.blend_mode(), 0, 0)
                    }
                }
            };
            let new_left = 0.max(-left) as u32;
            let new_top = 0.max(-top) as u32;
            let left = left.max(0) as u32;
            let top = top.max(0) as u32;
            let base_right = (left as u32 + new_image.width()).min(image.width());
            let base_bottom = (top as u32 + new_image.height()).min(image.height());
            let base_left = left.max(0) as u32;
            let base_top = top.max(0) as u32;

            let new_image = new_image.view(new_left, new_top, base_right - base_left, base_bottom - base_top);
            let mut image = image.sub_image(base_left, base_top, base_right - base_left, base_bottom - base_top);

            blend_image(&mut image, &new_image, BlendMode::try_from(blend_mode).expect(""));
        }
    }
    draw_inner(psd.layer_information().layer_info(), layer_tree, visible, &mut image, flip_x, flip_y);
    if flip_x {
        flip_horizontal_in_place(&mut image);
    }
    if flip_y {
        flip_vertical_in_place(&mut image);
    }
    image
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
enum BlendMode {
    Normal,
    Multiply,
    Lineardodge,
}

#[derive(Debug)]
struct BlendModeConvertError(ya_psd::layer_info::BlendMode);

impl TryFrom<ya_psd::layer_info::BlendMode> for BlendMode {
    type Error = BlendModeConvertError;

    fn try_from(value: ya_psd::layer_info::BlendMode) -> Result<Self, Self::Error> {
        match value {
            ya_psd::layer_info::BlendMode::Normal => Ok(BlendMode::Normal),
            ya_psd::layer_info::BlendMode::Multiply => Ok(BlendMode::Multiply),
            ya_psd::layer_info::BlendMode::Lineardodge => Ok(BlendMode::Lineardodge),
            _ => Err(BlendModeConvertError(value)),
        }
    }
}

impl BlendMode {
    fn get_raw_blend_function(self) -> fn(f64, f64) -> f64 {
        match self {
            BlendMode::Normal => {
                fn normal_blend(rgb_src: f64, _rgb_dest: f64) -> f64 {
                    rgb_src
                }
                normal_blend
            }
            BlendMode::Multiply => {
                fn multiply_blend(rgb_src: f64, rgb_dest: f64) -> f64 {
                    rgb_src * rgb_dest
                }
                multiply_blend
            }
            BlendMode::Lineardodge => {
                fn lineardodge_blend(rgb_src: f64, rgb_dest: f64) -> f64 {
                    rgb_src + rgb_dest
                }
                lineardodge_blend
            }
        }
    }
}

fn blend_image<B: GenericImage<Pixel = Rgba<u8>>, T: GenericImageView<Pixel = Rgba<u8>>>(image: &mut B, new_image: &T, blend_mode: BlendMode) {
    let raw_blend = blend_mode.get_raw_blend_function();
    for (x, y, Rgba([r1, g1, b1, a1])) in new_image.pixels() {
        let Rgba([r2, g2, b2, a2]) = image.get_pixel(x, y);
        let p = if a1 == 0 && a2 == 0 {
            Rgba::from([0, 0, 0, 0])
        } else {
            let r1 = r1 as f64 / 255f64;
            let g1 = g1 as f64 / 255f64;
            let b1 = b1 as f64 / 255f64;
            let a1 = a1 as f64 / 255f64;

            let r2 = r2 as f64 / 255f64;
            let g2 = g2 as f64 / 255f64;
            let b2 = b2 as f64 / 255f64;
            let a2 = a2 as f64 / 255f64;

            let a3 = a1 + a2 * (1. - a1);
            fn blend(rgb_src: f64, rgb_dest: f64, alpha_src: f64, alpha_dest: f64, alpha_result: f64, raw_blend: fn(f64, f64) -> f64) -> f64 {
                (alpha_src * (alpha_dest * raw_blend(rgb_src, rgb_dest) + (1. - alpha_dest) * rgb_src) + alpha_dest * rgb_dest * (1. - alpha_src)) / alpha_result
            }
            let r3 = blend(r1, r2, a1, a2, a3, raw_blend);
            let g3 = blend(g1, g2, a1, a2, a3, raw_blend);
            let b3 = blend(b1, b2, a1, a2, a3, raw_blend);

            let r = (r3 * 255.) as u8;
            let g = (g3 * 255.) as u8;
            let b = (b3 * 255.) as u8;
            let a = (a3 * 255.) as u8;

            Rgba::from([r, g, b, a])
        };
        image.put_pixel(x, y, p);
    }
}
