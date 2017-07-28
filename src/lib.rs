#[macro_use]
extern crate error_chain;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate serde_yaml;
extern crate inflector;
extern crate regex;
#[macro_use]
extern crate quote;
extern crate rustfmt;
#[macro_use]
extern crate lazy_static;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::ops::Deref;

use quote::{Tokens, ToTokens, Ident};
use inflector::Inflector;

use errors::*;

mod errors {
    error_chain!{
        foreign_links {
            Io(::std::io::Error);
            Json(::serde_json::Error);
            Yaml(::serde_yaml::Error);
            Utf8(::std::string::FromUtf8Error);
        }
    }
}

mod keywords;

lazy_static! {
    static ref RUST_KEYWORDS: HashSet<&'static str> = {
        keywords::RUST_KEYWORDS.iter().map(|v| *v).collect()
    };
}

const GENERIC_TYPE: &str = "serde_json::Value";

pub type PositiveInteger = i64;
pub type PositiveIntegerDefault0 = serde_json::Value;
pub type SchemaArray = Vec<Schema>;
pub type StringArray = Vec<String>;

type Map<T> = BTreeMap<String, T>;
type SchemaMap = BTreeMap<Uri, (SchemaType, Schema)>;

#[serde(rename = "simpleTypes")]
#[derive(Clone, Copy, PartialEq, Eq, Debug, Deserialize, Serialize, Hash)]
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

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Deserialize, Serialize)]
struct Uri(String);

impl Uri {
    fn is_definition(&self) -> bool {
        let split: Vec<_> = self.0.split("/").collect();
        let l = split.len();
        if l < 2 {
            false
        } else {
            split[l-2] == "defintions"
        }
    }

    fn join<T: fmt::Display>(&self, next: T) -> Uri {
        Uri(format!("{}/{}", self, next))
    }

    fn to_name<'a>(&self, root: &'a str) -> String {
        let rx = regex::Regex::new("^#").unwrap();
        let uri = rx.replace(&self.0, root);
        let name = uri.split("/").last().unwrap();
        match name.parse::<i32>() {
            // it was an array index (i.e. anonymous).
            // so just return the whole uri and do name mangling later
            // TODO where else might anonymous types occur?
            Ok(_) => uri.replace('/', " ").to_pascal_case(),
            _ => name.to_pascal_case(),
        }
    }

}

impl fmt::Display for Uri {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Uri {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}


impl Schema {
    /// find every instance in which a schema is defined or referenced
    fn gather_definitions(&self, uri: &Uri, schema_map: &mut SchemaMap) -> Result<()> {
        let schema_type = self.schema_type(uri)?;
        let exists = schema_map.insert(uri.clone(), (schema_type, self.clone()));
        if exists.is_some() {
            bail!("Schema already exists at location {}", uri)
        };
        gather_definitions_map(
            &self.definitions,
            uri.join("definitions"),
            schema_map,
        )?;
        gather_definitions_map(&self.properties, uri.join("properties"), schema_map)?;
        gather_definitions_map(
            &self.pattern_properties,
            uri.join("patternProperties"),
            schema_map,
        )?;
        gather_definitions_vec(&self.all_of, uri.join("allOf"), schema_map)?;
        gather_definitions_vec(&self.one_of, uri.join("oneOf"), schema_map)?;
        if let Some(ref schema) = self.not {
            schema.gather_definitions(
                &uri.join("/not"),
                schema_map,
            )?
        }
        if let Some(SchemaOrSchemas::Schema(ref schema)) = self.items {
            schema.gather_definitions(
                &uri.join("/items"),
                schema_map,
            )?
        }
        Ok(())
    }

    fn resolve<'a>(&'a self, uri: Uri, map: &'a SchemaMap) -> Result<(Uri, &'a Schema)> {
        // TODO implement from<&str> for Uri
        if let Some(ref ref_) = self.ref_ {
            let &(ref schema_type, ref deref) = map.get(&Uri(ref_.clone())).ok_or_else(|| {
                ErrorKind::from(format!("Failed to resolve reference '{}'", ref_))
            })?;
            deref.resolve(Uri(ref_.clone()), map)
        } else {
            Ok((uri, &self))
        }
    }

    fn typename(&self, root: &str, uri: &Uri, required: bool, map: &SchemaMap) -> String {
        // IFF it is anonymous AND it is primitive, return the name of the
        // primitive, else return the name from the uri
        let type_ = self
            .type_
            .as_ref()
            .and_then(|type_or_vec| match *type_or_vec {
                SimpleTypeOrSimpleTypes::SimpleType(st) => st.native_typename(),
                _ => None,  // We will need an enum
            })
            .map(|name| name.into())
            .unwrap_or(uri.to_name(root));
        if required {
            type_
        } else {
            format!("Option<{}>", type_)
        }
    }

    fn schema_type(&self, uri: &Uri) -> Result<SchemaType> {
        use SimpleType::*;
        if uri.is_definition() {
            return Ok(SchemaType::Defined)
        }
        if let Some(_) = self.ref_ {
            return Ok(SchemaType::Reference)
        }
        if let Some(ref st) = self.type_ {
            match st.unwrap_or_bail()? {
                Boolean | Integer | Null | Number | String => Ok(SchemaType::Primitive),
                Array => Ok(SchemaType::Array),
                Object => Ok(SchemaType::Object)
            }
        } else {
            Ok(SchemaType::Object)
        }
    }

    fn to_renderable(&self, root: &str, uri: &Uri, map: &SchemaMap) -> Result<Renderable> {
        println!("Rendering: {}", uri);
        use SimpleType::{Object, Boolean, Null, Integer, Number};
        let type_ = self.type_.as_ref().map(|t| t.unwrap_or_bail()).unwrap_or(Ok(Object))?;
        match type_ {
            Boolean | Integer | Null | Number | SimpleType::String => Ok(Renderable::Primitive),
            SimpleType::Array => {
                let tags = vec![];
                let inner = {
                    match self.items {
                        Some(SchemaOrSchemas::Schema(ref schema)) => {
                            schema.typename(root, uri, true, map)
                        }
                        Some(SchemaOrSchemas::Schemas(_)) => {
                            bail!("Multiple items not supported in schema {}", uri)
                        }
                        None => GENERIC_TYPE.into(),   // Unknown type, accept anything
                    }
                };
                let name = uri.to_name(root).to_string();
                Ok(Renderable::Array(Array { name, tags, inner }))
            }
            Object => self.renderable_object(root, uri, map)
        }
    }

    fn renderable_object(&self, root: &str, uri: &Uri, map: &SchemaMap) -> Result<Renderable> {
        let name = uri.to_name(root).to_string();

        if let Some(anyarr) = self.any_of.as_ref() {
            // we are going to make an enum
            let variants : Result<Vec<_>> = anyarr.iter().enumerate().map(|(ix, any)| {
                let uri = uri.join("anyOf").join(ix);
                Ok(any.typename(root, &uri, true, map))
            }).collect();
            return Ok(Renderable::Enum(Enum::new(name, vec![], variants?)?))
        }

        if let Some(allarr) = self.all_of.as_ref() {
            println!("ALL OF: {}", uri);
            let merged_schema = merge_schemas(allarr, map)?;
            return merged_schema.to_renderable(root, &uri, map)
        }

        let required_keys: HashSet<String> = self.required
            .as_ref()
            .map(|v| v.iter().map(|s| s.clone()).collect())
            .unwrap_or(HashSet::new());
        let fields = self.properties
            .as_ref()
            .unwrap_or(&Map::new())
            .iter()
            .map(|(name, schema)| {
                let is_required = required_keys.contains(name);
                let tags = vec![];
                let type_ = schema.typename(root, uri, is_required, map);
                Ok(Field::new(name.clone(), type_, tags)?)
            })
            .collect::<Result<Vec<Field>>>()?;
        let tags = vec![];
        Ok(Renderable::Struct(Struct::new(name, tags, fields)?))
    }
}

fn merge_schemas(schemas: &Vec<Schema>, map: &SchemaMap) -> Result<Schema> {
    let mut all_props = Map::<Schema>::new();
    let mut types = Vec::new();
    let _: () = schemas.iter().map(|schema| {
        let (_, real_schema) = schema.resolve(Uri("dummy".into()), map)?;
        if let Some(ref type_) = real_schema.type_ {
            types.push(type_.unwrap_or_bail()?)
        };
        real_schema.properties.as_ref().map(|map| {
            map.iter().map(|(name, propschema)| {
                match all_props.insert(name.clone(), propschema.clone()) {
                    Some(_) => bail!("Duplicate key found: {}", name),
                    None => Ok(())
                }
            }).collect::<Result<Vec<_>>>().map(|_| ())
        }).unwrap_or(Ok(()))
    }).collect::<Result<Vec<()>>>().map(|_| ())?;
    let mut newschema: Schema = Default::default();
    let type_ = {
        let dedup: HashSet<_> = types.iter().collect();
        match dedup.len() {
            0 => SimpleType::Object,
            1 => *types.first().unwrap(),
            _ => bail!("Inconsistent types for allOf: {:?}", types)
        }
    };
    newschema.type_ = Some(SimpleTypeOrSimpleTypes::SimpleType(type_));
    newschema.properties = if all_props.len() > 0 {
        Some(all_props)
    } else {
        None
    };
    Ok(newschema)
}

fn gather_definitions_map(
    maybe_schma_map: &Option<Map<Schema>>,
    uri: Uri,
    schema_map: &mut SchemaMap,
) -> Result<()> {
    if let Some(ref map) = *maybe_schma_map {
        for (name, schema) in map {
            schema.gather_definitions(&uri.join(name), schema_map)?;
        }
    }
    Ok(())
}

fn gather_definitions_vec(
    maybe_schma_vec: &Option<SchemaArray>,
    uri: Uri,
    schema_map: &mut SchemaMap,
) -> Result<()> {
    if let Some(ref schemas) = *maybe_schma_vec {
        for (ix, schema) in schemas.iter().enumerate() {
            schema.gather_definitions(&uri.join(ix), schema_map)?;
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
    fn gather_definitions(&self) -> Result<SchemaMap> {
        let mut map = SchemaMap::new();
        self.0.gather_definitions(&Uri("#".into()), &mut map)?;
        Ok(map)
    }

    fn renderables(&self, root: &str) -> Result<Vec<Renderable>> {
        let map = self.gather_definitions()?;
        let defns = resolve_definitions(&map);
        defns
            .iter()
            .map(|&(uri, &(_, ref schema))| schema.to_renderable(root, uri, &map))
            .collect()
    }

    fn render_all(&self, root: &str) -> Result<Tokens> {
        let renderables = self.renderables(root)?;
        Ok(render_all(&renderables))
    }

    pub fn generate(&self, root: &str) -> Result<String> {
        let tokens = self.render_all(root)?;
        beautify(tokens)
    }
}

fn render_all(renderables: &Vec<Renderable>) -> Tokens {
    renderables.iter().fold(
        Tokens::new(),
        |mut tokens, renderable| {
            renderable.to_tokens(&mut tokens);
            tokens
        },
    )
}

fn resolve_definitions(map: &SchemaMap) -> Vec<(&Uri, &(SchemaType, Schema))> {
    // Filter out schemas which are just references to other schemas
    map.iter()
        .filter(|&(_uri, &(_, ref schema))| schema.ref_.is_none())
        .map(|(uri, schema)| (uri, schema))
        .collect()
}

type Schemas = Vec<Schema>;

enum SchemaType {
    Reference,
    Primitive,
    Array,
    Object,
    Defined,
}

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

impl SimpleTypeOrSimpleTypes {
    fn unwrap_or_bail(&self) -> Result<SimpleType> {
        match *self {
            SimpleTypeOrSimpleTypes::SimpleType(st) => Ok(st),
            _ => bail!("Multiple types not supported"),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum BoolOrSchema {
    Bool(bool),
    Schema(Box<Schema>),
}

#[derive(Clone, PartialEq, Debug)]
enum Renderable {
    Primitive,
    Array(Array),
    Enum(Enum),
    Struct(Struct),
}

impl ToTokens for Renderable {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use Renderable::*;
        match *self {
            Primitive => {}  // No need to render a primitive
            Array(ref elem) => elem.to_tokens(tokens),
            Enum(ref elem) => elem.to_tokens(tokens),
            Struct(ref elem) => elem.to_tokens(tokens),
        }
    }
}

fn make_valid_identifier(s: String) -> Result<String> {
    // strip out invalid characters and ensure result is valid
    // bit ugly to reallocate but at least it is simple
    // TODO use unicode XID_start/XID_continue
    let mut out = String::new();
    let mut is_leading_char = true;
    for c in s.chars() {
        if is_leading_char {
            match c {
                'A'...'Z' | 'a'...'z' | '_' => {
                    is_leading_char = false;
                    out.push(c);
                }
                _ => (),
            }
        } else {
            match c {
                'A'...'Z' | 'a'...'z' | '_' | '0'...'9' => out.push(c),
                _ => (),
            }
        }
    }
    if RUST_KEYWORDS.contains(&*out) {
        out.push('_')
    };
    if out.len() == 0 || out == "_" {
        bail!("could not generate valid identifier from {}", s)
    }
    Ok(out)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    name: String,
    tags: Vec<Tokens>,
    fields: Vec<Field>,
}

impl Struct {
    fn new(name: String, mut tags: Vec<Tokens>, fields: Vec<Field>) -> Result<Struct> {
        let name = make_valid_identifier(name.to_pascal_case())?;
        tags.push(quote! {
            #[derive(Debug, Clone, Default, PartialEq)]
        });
        Ok(Struct { name, tags, fields })
    }
}

impl ToTokens for Struct {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let fields = &self.fields;
        let tags: Vec<_> = self.tags.iter().map(|t| Ident::new(t.as_str())).collect();
        let tok =
            quote! {
            #(#tags),*
            pub struct #name {
                #(#fields),*

            }
        };
        tokens.append(tok)
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Field {
    name: String,
    type_: String,
    tags: Vec<Tokens>,
}

impl Field {
    fn new(name: String, type_: String, mut tags: Vec<Tokens>) -> Result<Field> {
        let snake = make_valid_identifier(name.to_snake_case())?;
        if name != snake {
            tags.push(quote!{
                #[serde(rename = #name)]
            })
        }
        Ok(Field {
            name: snake,
            type_,
            tags,
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags: Vec<_> = self.tags.iter().map(|t| Ident::new(t.as_str())).collect();
        let type_ = Ident::new(&*self.type_);
        let tok =
            quote! {
            #(#tags),*
            #name: #type_
        };
        tokens.append(tok);
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Enum {
    name: String,
    tags: Vec<Tokens>,
    variants: Vec<(String, String)>,
}

impl Enum {
    fn new(name: String, tags: Vec<Tokens>, variants: Vec<String>) -> Result<Enum> {
        let name = make_valid_identifier(name.to_pascal_case())?;
        let variants = variants.into_iter().map(|v| {
            let valid = make_valid_identifier(v)?;
            Ok((valid.clone(), valid))
        }).collect::<Result<Vec<_>>>()?;
        Ok(Enum { name, tags, variants })
    }
}

impl ToTokens for Enum {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags = &self.tags;
        let names = self.variants.iter().map(|v| Ident::new(&*v.0.to_pascal_case()));
        let types = self.variants.iter().map(|v| Ident::new(&*v.1));
        let tok =
            quote! {
            #(#tags),*
            pub enum #name {
                #(#names(#types)),*
            }
        };
        tokens.append(tok);
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct Array {
    name: String,
    tags: Vec<String>,
    inner: String,
}

impl ToTokens for Array {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags: Vec<_> = self.tags.iter().map(|t| Ident::new(t.as_str())).collect();
        let inner = Ident::new(&*self.inner);
        let tok =
            quote! {
            #(#tags),*
            type #name = Vec<#inner>;
        };
        tokens.append(tok);
    }
}

fn beautify(t: Tokens) -> Result<String> {
    use rustfmt::*;

    // FIXME workaround is necessary until rustfmt works programmatically
    let tmppath = "/tmp/rustfmt.rs";  // TODO use tempdir
    {
        let mut tmp = File::create(tmppath)?;
        tmp.write_all(t.as_str().as_bytes())?;
    }

    let input = Input::File(tmppath.into());
    let mut fakebuf = Vec::new();  // pretty weird that this is necessary.. but it is

    match format_input(input, &Default::default(), Some(&mut fakebuf)) {
        Ok((_summmary, _filemap, _report)) => {}
        Err((e, _summary)) => Err(e)?,
    }

    let mut tmp = File::open(tmppath)?;
    let mut buf = String::new();
    tmp.read_to_string(&mut buf)?;
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn metaschema() -> RootSchema {
        RootSchema::from_file_json("test_schemas/metaschema-draft4.json").unwrap()
        //RootSchema::from_file_yaml("test_schemas/openapi3-schema.yaml").unwrap()
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
        assert_eq!(map.len(), 42)
    }

    #[test]
    fn test_gather_renderables() {
        let root = metaschema();
        root.renderables("root").unwrap();
    }

    #[test]
    fn test_generate() {
        let root = metaschema();
        let _code = root.generate("root").unwrap();
    }

    #[test]
    fn test_make_valid_identifier() {
        let id1 = "1234_abcd".into();
        assert_eq!(make_valid_identifier(id1).unwrap(), "_abcd");
        let id2 = "$1234Abcd".into();
        assert_eq!(make_valid_identifier(id2).unwrap(), "Abcd");
        let id3 = "$@1234\\|./".into();
        assert!(make_valid_identifier(id3).is_err());
        let id4 = "1234_".into();
        assert!(make_valid_identifier(id4).is_err());
        let id5 = "".into();
        assert!(make_valid_identifier(id5).is_err());
        let id6 = "_".into();
        assert!(make_valid_identifier(id6).is_err());
        let id7 = "type".into();
        assert_eq!(make_valid_identifier(id7).unwrap(), "type_");
        let id8 = "this 123".into();
        assert_eq!(make_valid_identifier(id8).unwrap(), "this123");
    }

    #[test]
    fn test_uri_to_name() {
        let id1 = Uri("#/this/is/some/route/my type".into());
        assert_eq!(id1.to_name("root"), "MyType");
        let id2 = Uri("#/some/other/route/1".into());
        assert_eq!(id2.to_name("root"), "RootSomeOtherRoute1");
    }

    #[test]
    fn test_simple_schema() {
        let root = RootSchema::from_file_yaml("test_schemas/simple.yaml").unwrap();
        root.generate("Test").unwrap();
    }
}
