use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::RwLock;

use actix_web::dev::HttpResponseBuilder;
use actix_web::{get, post, web, App, HttpServer, Responder};

use actix_web::http::StatusCode;
use actix_web::web::Json;
use clap::{crate_authors, crate_description, crate_name, crate_version, Arg};
use image::png::PngEncoder;
use image::ColorType;
use once_cell::sync::OnceCell;
use psdtool_lib::ya_psd;
use psdtool_lib::{PsdTool, PsdToolController};
use serde_derive::Deserialize;

#[derive(Debug, Deserialize)]
struct PsdSettingsQuery {
    settings: Option<String>,
    #[serde(default)]
    flip_x: bool,
    #[serde(default)]
    flip_y: bool,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
enum CharacterImageSetting {
    #[serde(alias = "favorite")]
    Favorite { path: String },
    #[serde(alias = "layer")]
    Layer { path: String },
}

#[derive(Debug, Deserialize)]
struct CharacterImageSettingsPostData {
    #[serde(default)]
    settings: Vec<CharacterImageSetting>,
    #[serde(default)]
    flip_x: bool,
    #[serde(default)]
    flip_y: bool,
}

#[get("/image/{name}")]
async fn character_image_get(web::Path(name): web::Path<String>, web::Query(PsdSettingsQuery { settings, flip_x, flip_y }): web::Query<PsdSettingsQuery>) -> impl Responder {
    let settings = dbg!(settings).and_then(|settings| serde_json::from_str::<Vec<CharacterImageSetting>>(&settings).ok()).unwrap_or_default();
    generate_character_image(&name, settings, flip_x, flip_y).await
}

#[post("/image/{name}")]
async fn character_image_post(web::Path(name): web::Path<String>, web::Json(CharacterImageSettingsPostData { settings, flip_x, flip_y }): web::Json<CharacterImageSettingsPostData>) -> impl Responder {
    generate_character_image(&name, settings, flip_x, flip_y).await
}

async fn generate_character_image(name: &str, settings: Vec<CharacterImageSetting>, flip_x: bool, flip_y: bool) -> impl Responder {
    dbg!(name);
    let (psd, cache) = CHARACTER_PSD_DATA.get().unwrap().get(name)?;
    let mut psd = psd.clone_ref();
    if flip_x {
        psd.flip_x();
    }
    if flip_y {
        psd.flip_y();
    }
    for setting in dbg!(settings) {
        match setting {
            CharacterImageSetting::Favorite { path } => {
                dbg!(psd.apply_favorite(&path)).ok()?;
            }
            CharacterImageSetting::Layer { path } => {
                dbg!(psd.switch_layer_by_string_path(&path)).ok()?;
            }
        }
    }
    let result = cache
        .write()
        .unwrap()
        .entry((psd.get_visible().to_vec(), psd.is_flip_x(), psd.is_flip_y()))
        .or_insert_with(|| {
            let image = psd.draw();
            let mut result = Vec::new();
            let encoder = PngEncoder::new(&mut result);
            encoder.encode(image.as_raw(), image.width(), image.height(), ColorType::Rgba8).unwrap();
            result
        })
        .clone();

    Some(HttpResponseBuilder::new(StatusCode::OK).content_type("image/png").body(result))
}

#[get("/character_names")]
async fn character_names() -> impl Responder {
    Json(CHARACTER_PSD_DATA.get().unwrap().keys().collect::<Vec<_>>())
}

#[get("/layers/{name}")]
async fn character_layers(web::Path(name): web::Path<String>) -> impl Responder {
    let (psd, _) = CHARACTER_PSD_DATA.get().unwrap().get(&name)?;
    Some(Json(get_layer_list(psd)))
}

fn get_layer_list(psd: &PsdToolController) -> Vec<String> {
    psd.layer_list().into_iter().map(|name| name.into_vec().into_iter().map(|name| name.into_string()).reduce(|a, b| format!("{}/{}", a, b)).unwrap()).collect::<Vec<_>>()
}

#[get("/favorites/{name}")]
async fn character_favorites(web::Path(name): web::Path<String>) -> impl Responder {
    let (psd, _) = CHARACTER_PSD_DATA.get().unwrap().get(&name)?;
    Some(Json(psd.favorite_list()))
}

static CHARACTER_PSD_DATA: OnceCell<HashMap<String, (PsdToolController<'static>, RwLock<HashMap<(Vec<bool>, bool, bool), Vec<u8>>>)>> = OnceCell::new();

#[actix_web::main]
async fn main() -> anyhow::Result<()> {
    let matches = clap::app_from_crate!().arg(Arg::with_name("setting_json").takes_value(true).required(true)).get_matches();
    let setting_json = matches.value_of("setting_json").unwrap();
    let setting_json_file = std::fs::read_to_string(setting_json);
    let setting_json = setting_json_file.as_deref().unwrap_or(setting_json);
    let result = serde_json::from_str::<HashMap<String, PsdSettings>>(setting_json)?;
    let mut character_psd_data = HashMap::new();
    for (character_name, PsdSettings { psd, pfv, layers_out, favorites_out }) in result {
        let psd = std::fs::read(psd)?;
        let psd = ya_psd::parse_psd(&psd)?;
        let psd = psd.into_static();
        let controller = if let Some(pfv) = pfv {
            let pfv = std::fs::File::open(pfv)?;
            PsdToolController::with_pfv_file(psd, pfv)?
        } else {
            PsdToolController::new(psd)
        };
        if let Some(layers_out) = layers_out {
            if let Some(directory) = layers_out.parent() {
                std::fs::create_dir_all(directory)?;
            }
            std::fs::write(layers_out, serde_json::to_string(&get_layer_list(&controller))?)?;
        }
        if let Some(favorites_out) = favorites_out {
            if let Some(directory) = favorites_out.parent() {
                std::fs::create_dir_all(directory)?;
            }
            std::fs::write(favorites_out, serde_json::to_string(&controller.favorite_list())?)?;
        }
        character_psd_data.insert(character_name, (controller, Default::default()));
    }
    CHARACTER_PSD_DATA.set(character_psd_data).unwrap();

    HttpServer::new(|| App::new().service(character_names).service(character_layers).service(character_favorites).service(character_image_get).service(character_image_post)).bind("127.0.0.1:8080")?.run().await?;
    Ok(())
}

#[derive(Deserialize)]
struct PsdSettings {
    psd: PathBuf,
    pfv: Option<PathBuf>,
    layers_out: Option<PathBuf>,
    favorites_out: Option<PathBuf>,
}
