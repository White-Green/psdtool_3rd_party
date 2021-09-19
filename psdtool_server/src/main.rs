use std::collections::HashMap;

use actix_web::{App, get, HttpServer, post, Responder, web};
use actix_web::dev::HttpResponseBuilder;

use actix_web::http::StatusCode;
use actix_web::web::Json;
use clap::{Arg, crate_authors, crate_description, crate_name, crate_version};
use image::ColorType;
use image::png::PngEncoder;
use once_cell::sync::{OnceCell};
use serde_derive::Deserialize;

use psdtool_lib::{PsdTool, PsdToolController};

#[derive(Deserialize)]
struct PsdSettingsQuery {
    settings: Option<String>,
}

#[derive(Deserialize)]
#[serde(tag = "type")]
enum CharacterImageSetting {
    #[serde(alias = "favorite")]
    Favorite { path: String },
    #[serde(alias = "layer")]
    Layer { path: String },
}

#[get("/image/{name}")]
async fn character_image_get(web::Path(name): web::Path<String>, web::Query(PsdSettingsQuery { settings }): web::Query<PsdSettingsQuery>) -> impl Responder {
    let settings = settings.and_then(|settings| serde_json::from_str::<Vec<CharacterImageSetting>>(&settings).ok()).unwrap_or_default();
    generate_character_image(&name, settings).await
}

#[post("/image/{name}")]
async fn character_image_post(web::Path(name): web::Path<String>, web::Json(settings): web::Json<Vec<CharacterImageSetting>>) -> impl Responder {
    generate_character_image(&name, settings).await
}

async fn generate_character_image(name: &str, settings: Vec<CharacterImageSetting>) -> impl Responder {
    let psd = CHARACTER_PSD_DATA.get().unwrap().get(name)?;
    let mut psd = psd.clone_ref();
    for setting in settings {
        match setting {
            CharacterImageSetting::Favorite { path } => {
                psd.apply_favorite(&path).ok()?;
            }
            CharacterImageSetting::Layer { path } => {
                psd.switch_layer_by_string_path(&path).ok()?;
            }
        }
    }
    let image = psd.draw();
    let mut result = Vec::new();
    let encoder = PngEncoder::new(&mut result);
    encoder.encode(image.as_raw(), image.width(), image.height(), ColorType::Rgba8).ok()?;
    Some(HttpResponseBuilder::new(StatusCode::OK)
        .content_type("image/png")
        .body(result))
}

#[get("/character_names")]
async fn character_names() -> impl Responder {
    Json(CHARACTER_PSD_DATA.get().unwrap().keys().collect::<Vec<_>>())
}

static CHARACTER_PSD_DATA: OnceCell<HashMap<String, PsdToolController<'static>>> = OnceCell::new();

#[actix_web::main]
async fn main() -> anyhow::Result<()> {
    let matches = clap::app_from_crate!()
        .arg(Arg::with_name("setting_json").takes_value(true).required(true))
        .get_matches();
    let setting_json = matches.value_of("setting_json").unwrap();
    let setting_json_file = std::fs::read_to_string(setting_json);
    let setting_json = setting_json_file.as_deref().unwrap_or(setting_json);
    let result = serde_json::from_str::<HashMap<String, PsdSettings>>(setting_json)?;
    let mut character_psd_data = HashMap::new();
    for (character_name, PsdSettings { psd, pfv }) in result {
        let psd = std::fs::read(psd)?;
        let psd = ya_psd::parse_psd(&psd)?;
        let psd = psd.into_static();
        let controller = if let Some(pfv) = pfv {
            let pfv = std::fs::File::open(pfv)?;
            PsdToolController::with_pfv_file(psd, pfv)?
        } else {
            PsdToolController::new(psd)
        };
        character_psd_data.insert(character_name, controller);
    }
    CHARACTER_PSD_DATA.set(character_psd_data).unwrap();

    HttpServer::new(||
        App::new()
            .service(character_names)
            .service(character_image_get)
            .service(character_image_post))
        .bind("127.0.0.1:8080")?
        .run()
        .await?;
    Ok(())
}

#[derive(Deserialize)]
struct PsdSettings {
    psd: String,
    pfv: Option<String>,
}
