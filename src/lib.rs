#[macro_use]
extern crate error_chain;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate serde_yaml;
extern crate inflector;
extern crate quote;
extern crate rustfmt;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::collections::BTreeMap;
use std::fmt;

use errors::*;

mod errors {
    error_chain!{
        foreign_links {
            Io(::std::io::Error);
            Json(::serde_json::Error);
            Yaml(::serde_yaml::Error);
        }
    }
}

static RUST_KEYWORDS: &'static [&'static str] = &[
    "as", "break", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "static", "self", "Self", "struct", "super", "true", "trait",
    "type", "unsafe", "use", "while", "continue", "box", "const", "where", "virtual",
    "proc", "alignof", "become", "offsetof", "priv", "pure", "sizeof", "typeof",
    "unsized", "yield", "do", "abstract", "final", "override"
];

pub type PositiveInteger = i64;
pub type PositiveIntegerDefault0 = serde_json::Value;
pub type SchemaArray = Vec<Schema>;
pub type StringArray = Vec<String>;

type Map<T> = BTreeMap<String, T>;

#[serde(rename = "simpleTypes")]
#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub enum SimpleTypes {
    #[serde(rename = "array")]
    Array,
    #[serde(rename = "boolean")]
    Boolean,
    #[serde(rename = "integer")]
    Integer,
    #[serde(rename = "null")]
    Null,
    #[serde(rename = "number")]
    Number,
    #[serde(rename = "object")]
    Object,
    #[serde(rename = "string")]
    String,
}

#[derive(Clone, PartialEq, Debug, Default, Deserialize, Serialize)]
pub struct Schema {
    #[serde(rename = "$ref")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ref_: Option<String>,
    #[serde(rename = "$schema")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schema: Option<String>,
    #[serde(rename = "additionalItems")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_items: Option<serde_json::Value>,
    #[serde(rename = "additionalProperties")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_properties: Option<BoolOrSchema>,
    #[serde(rename = "allOf")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub all_of: Option<SchemaArray>,
    #[serde(rename = "anyOf")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub any_of: Option<SchemaArray>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definitions: Option<Map<Schema>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<Map<serde_json::Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "enum")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enum_: Option<Vec<serde_json::Value>>,
    #[serde(rename = "exclusiveMaximum")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exclusive_maximum: Option<bool>,
    #[serde(rename = "exclusiveMinimum")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exclusive_minimum: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<SchemaOrSchemas>,
    #[serde(rename = "maxItems")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_items: Option<PositiveInteger>,
    #[serde(rename = "maxLength")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_length: Option<PositiveInteger>,
    #[serde(rename = "maxProperties")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_properties: Option<PositiveInteger>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub maximum: Option<f64>,
    #[serde(rename = "minItems")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_items: Option<PositiveIntegerDefault0>,
    #[serde(rename = "minLength")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_length: Option<PositiveIntegerDefault0>,
    #[serde(rename = "minProperties")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_properties: Option<PositiveIntegerDefault0>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub minimum: Option<f64>,
    #[serde(rename = "multipleOf")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub multiple_of: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub not: Option<Box<Schema>>,
    #[serde(rename = "oneOf")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub one_of: Option<SchemaArray>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pattern: Option<String>,
    #[serde(rename = "patternProperties")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pattern_properties: Option<Map<Schema>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Map<Schema>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<StringArray>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(rename = "type")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub type_: Option<SimpleTypesOrSimpleTypes1>,
    #[serde(rename = "uniqueItems")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unique_items: Option<bool>,
}

impl fmt::Display for Schema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", serde_json::to_string_pretty(self).unwrap())
    }
}


impl Schema {
    pub fn from_reader_yaml<R: Read>(reader: R) -> Result<Schema> {
        Ok(serde_yaml::from_reader(reader)?)
    }

    pub fn from_file_yaml<P: AsRef<Path>>(path: P) -> Result<Schema> {
        Schema::from_reader_yaml(File::open(path)?)
    }

    pub fn from_reader_json<R: Read>(reader: R) -> Result<Schema> {
        Ok(serde_json::from_reader(reader)?)
    }

    pub fn from_file_json<P: AsRef<Path>>(path: P) -> Result<Schema> {
        Schema::from_reader_json(File::open(path)?)
    }

    pub fn gather_definitions(&self, path: &str, schema_map: &mut Map<Schema>) -> Result<()> {
        gather_definitions(&self.definitions, &format!("{}/definitions", path), schema_map)?;
        gather_definitions(&self.properties, &format!("{}/properties", path), schema_map)?;
        gather_definitions(&self.pattern_properties, &format!("{}/pattern_properties", path), schema_map)?;
        Ok(())
    }

}

fn gather_definitions(maybe_schma_map: &Option<Map<Schema>>, path: &str, schema_map: &mut Map<Schema>) -> Result<()> {
    if let Some(ref map) = *maybe_schma_map {
        for (name, prop) in map {
            let current_path = format!("{}/{}", path, name);
            let previous = schema_map.insert(current_path.clone(), prop.clone());
            if previous.is_some() {
                bail!("Schema already exists at location {}", current_path)
            };
            prop.gather_definitions(&current_path, schema_map)?;
        }
    }
    Ok(())
}

type Schemas = Vec<Schema>;

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SchemaOrSchemas {
    Schema(Box<Schema>),
    Schemas(Schemas)
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SimpleTypesOrSimpleTypes1 {
    SimpleTypes(SimpleTypes),
    SimpleTypes1(Vec<SimpleTypes>)
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum BoolOrSchema {
    Bool(bool),
    Schema(Box<Schema>)
}

enum StructName {
    Named(String),
    Anonymous
}

pub struct Struct {
    name: StructName,
    tags: Vec<String>,
    fields: Vec<Field>
}

pub struct Field {
    name: String,
    tags: Vec<String>,
}

struct UntaggedEnumWrapper {
    variants: Vec<String>
}


#[cfg(test)]
mod tests {
    use super::*;

    fn metaschema() -> Schema {
        Schema::from_file_json("metaschema-draft4.json").unwrap()
    }

    #[test]
    fn load_openapi3_schema() {
         Schema::from_file_yaml("openapi3-schema.yaml").unwrap();
    }

    #[test]
    fn load_meta_schema() {
        Schema::from_file_json("metaschema-draft4.json").unwrap();
    }

    #[test]
    fn gather_schemas() {
        let schema = metaschema();
        let mut map = Map::new();
        schema.gather_definitions("#".into(), &mut map).unwrap();
        for (e, s) in map {
            println!("{}: {}", e, s)
        }
    }
}
