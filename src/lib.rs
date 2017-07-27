#[macro_use]
extern crate error_chain;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
extern crate serde_yaml;
extern crate inflector;
#[macro_use]
extern crate quote;
extern crate rustfmt;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::collections::BTreeMap;
use std::fmt;
use quote::{Tokens, ToTokens};

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

// TODO: lazy-static this
mod keywords;

pub type PositiveInteger = i64;
pub type PositiveIntegerDefault0 = serde_json::Value;
pub type SchemaArray = Vec<Schema>;
pub type StringArray = Vec<String>;

type Map<T> = BTreeMap<String, T>;

#[serde(rename = "simpleTypes")]
#[derive(Clone, Copy, PartialEq, Debug, Deserialize, Serialize)]
pub enum SimpleType {
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

impl SimpleType {
    fn native_typename(&self) -> Option<&'static str> {
        use SimpleType::*;
        let name = match *self {
            Null => "()",
            Boolean => "bool",
            Integer => "i64",
            Number => "f64",
            String => "String",
            Object | Array => return None,
        };
        Some(name)
    }

    fn is_primitive(&self) -> bool {
        use SimpleType::*;
        match *self {
            Object | Array => false,
            _ => true,
        }
    }
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
    pub type_: Option<SimpleTypeOrSimpleTypes>,
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
    /// find every instance in which a schema is defined or referenced
    pub fn gather_definitions(&self, path: String, schema_map: &mut Map<Schema>) -> Result<()> {
        let exists = schema_map.insert(path.clone(), self.clone());
        if exists.is_some() {
            bail!("Schema already exists at location {}", path)
        };
        gather_definitions_map(
            &self.definitions,
            format!("{}/definitions", path),
            schema_map,
        )?;
        gather_definitions_map(&self.properties, format!("{}/properties", path), schema_map)?;
        gather_definitions_map(
            &self.pattern_properties,
            format!("{}/patternProperties", path),
            schema_map,
        )?;
        gather_definitions_vec(&self.all_of, format!("{}/allOf", path), schema_map)?;
        gather_definitions_vec(&self.any_of, format!("{}/anyOf", path), schema_map)?;
        gather_definitions_vec(&self.one_of, format!("{}/oneOf", path), schema_map)?;
        if let Some(ref schema) = self.not {
            schema.gather_definitions(
                format!("{}/not", path),
                schema_map,
            )?
        }
        Ok(())
    }

    // TODO is this function actually necessary?
    fn resolve<'a>(&'a self, uri: &'a str, map: &'a Map<Schema>) -> Result<(&'a str, &'a Schema)> {
        if let Some(ref ref_) = self.ref_ {
            let deref = map.get(ref_).ok_or_else(|| {
                ErrorKind::from(format!("Failed to resolve reference '{}'", ref_))
            })?;
            deref.resolve(ref_, map)
        } else {
            Ok((uri, &self))
        }
    }

    fn typename(&self, uri: &str, map: &Map<Schema>) -> Result<String> {
        // assume it is already validated
        let (realuri, realschema) = self.resolve(uri, map)?;
        Ok(realschema.typename_(realuri))
    }

    fn typename_(&self, uri: &str) -> String {
        self.type_
            .as_ref()
            .and_then(|type_or_vec| match *type_or_vec {
                SimpleTypeOrSimpleTypes::SimpleType(st) => st.native_typename(),
                _ => None,  // We will need an enum
            })
            .map(|name| name.into())
            .unwrap_or("JsonValue".into())
    }

    fn is_primitive(&self) -> bool {
        self.type_
            .as_ref()
            .map(|type_or_vec| match *type_or_vec {
                SimpleTypeOrSimpleTypes::SimpleType(st) => st.is_primitive(),
                _ => false,
            })
            .unwrap_or(false)
    }

    fn validate(&self) -> Result<()> {
        // TODO validation
        unimplemented!()
    }


    fn renderable(&self, uri: &str, map: &Map<Schema>) -> Result<SchemaType> {
        if self.is_primitive() {
            return Ok(SchemaType::Primitive);
        }
        if self.properties.is_none() {
            return Ok(SchemaType::Primitive);
        }
        let tags = vec![];
        let fields = self.properties
            .as_ref()
            .unwrap()
            .iter()
            .map(|(name, schema)| {
                let tags = vec![];
                let type_ = schema.typename(uri, map)?;
                Ok(Field {
                    name: name.clone(),
                    type_,
                    tags,
                })
            })
            .collect::<Result<Vec<Field>>>()?;
        let name = uri.into(); // TODO
        Ok(SchemaType::Struct(Struct { name, tags, fields }))
    }
}

fn gather_definitions_map(
    maybe_schma_map: &Option<Map<Schema>>,
    path: String,
    schema_map: &mut Map<Schema>,
) -> Result<()> {
    if let Some(ref map) = *maybe_schma_map {
        for (name, schema) in map {
            let next_path = format!("{}/{}", path, name);
            schema.gather_definitions(next_path, schema_map)?;
        }
    }
    Ok(())
}

fn gather_definitions_vec(
    maybe_schma_vec: &Option<SchemaArray>,
    path: String,
    schema_map: &mut Map<Schema>,
) -> Result<()> {
    if let Some(ref schemas) = *maybe_schma_vec {
        for (ix, schema) in schemas.iter().enumerate() {
            let next_path = format!("{}/{}", path, ix);
            schema.gather_definitions(next_path, schema_map)?;
        }
    }
    Ok(())
}

#[derive(Clone, PartialEq, Debug, Default, Deserialize, Serialize)]
pub struct RootSchema(Schema);

impl RootSchema {
    pub fn from_reader_yaml<R: Read>(reader: R) -> Result<RootSchema> {
        Ok(serde_yaml::from_reader(reader)?)
    }

    pub fn from_file_yaml<P: AsRef<Path>>(path: P) -> Result<RootSchema> {
        RootSchema::from_reader_yaml(File::open(path)?)
    }

    pub fn from_reader_json<R: Read>(reader: R) -> Result<RootSchema> {
        Ok(serde_json::from_reader(reader)?)
    }

    pub fn from_file_json<P: AsRef<Path>>(path: P) -> Result<RootSchema> {
        RootSchema::from_reader_json(File::open(path)?)
    }
}

impl RootSchema {
    fn gather_definitions(&self) -> Result<Map<Schema>> {
        let mut map = Map::new();
        self.0.gather_definitions("#".into(), &mut map)?;
        Ok(map)
    }

    fn renderables(&self) -> Result<Vec<Struct>> {
        let map = self.gather_definitions()?;
        map.iter()
            .map(|(uri, schema)| schema.renderable(uri, &map))
            .filter_map(|renderable| match renderable {
                Err(e) => Some(Err(e)),
                Ok(SchemaType::Struct(s)) => Some(Ok(s)),
                Ok(_) => None,
            })
            .collect()
    }

    pub fn render_all(&self) -> Result<Tokens> {
        let renderables = self.renderables()?;
        Ok(render_all(&renderables))
    }
}

fn render_all(renderables: &Vec<Struct>) -> Tokens {
    renderables.iter().fold(Tokens::new(), |mut tokens, renderable| {
        renderable.to_tokens(&mut tokens);
        tokens
    })
}

fn resolve_definitions(map: &Map<Schema>) -> Vec<(&str, &Schema)> {
    // delivers the list of schemas we actually need to create
    map.iter()
        .filter(|&(_uri, schema)| schema.ref_.is_none())
        .map(|(uri, schema)| (uri.as_str(), schema))
        .collect()
}

type Schemas = Vec<Schema>;

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SchemaOrSchemas {
    Schema(Box<Schema>),
    Schemas(Schemas),
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SimpleTypeOrSimpleTypes {
    SimpleType(SimpleType),
    SimpleTypes(Vec<SimpleType>),
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum BoolOrSchema {
    Bool(bool),
    Schema(Box<Schema>),
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
enum SchemaType {
    Primitive,
    Array,
    Enum(Enum),
    Struct(Struct),
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct Struct {
    name: String,
    tags: Vec<String>,
    fields: Vec<Field>,
}

impl ToTokens for Struct {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let fields = &self.fields;
        let tags = &self.tags;
        let tok = quote! {
            #(#tags),*
            pub struct #name {
                #(#fields),*

            }
        };
        tokens.append(tok)
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct Field {
    name: String,
    type_: String,
    tags: Vec<String>,
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tags = &self.tags;
        let type_ = &self.type_;
        let tok = quote! {
            #(#tags),*
            #name: #type_
        };
        tokens.append(tok);
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
struct Enum {
    variants: Vec<String>,
}


#[cfg(test)]
mod tests {
    use super::*;

    fn metaschema() -> RootSchema {
        RootSchema::from_file_json("test_schemas/metaschema-draft4.json").unwrap()
    }

    #[test]
    fn load_openapi3_schema() {
        RootSchema::from_file_yaml("test_schemas/openapi3-schema.yaml").unwrap();
    }

    #[test]
    fn load_meta_schema() {
        RootSchema::from_file_json("test_schemas/metaschema-draft4.json").unwrap();
    }

    #[test]
    fn gather_schemas() {
        let root = metaschema();
        let map = root.gather_definitions().unwrap();
        assert_eq!(map.len(), 48)
    }

    #[test]
    fn test_gather_renderables() {
        let root = metaschema();
        let renderable = root.renderables().unwrap();
        assert_eq!(renderable.len(), 1)
    }

    #[test]
    fn test_render() {
        let root = metaschema();
        let tokens = root.render_all().unwrap();
        println!("{}", tokens)
    }
}
