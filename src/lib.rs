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
#[macro_use]
extern crate derive_new;

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
type SchemaMap = BTreeMap<Uri, MetaSchema>;

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

impl<'a> From<&'a str> for Uri {
    fn from(s: &str) -> Uri {
        Uri(s.into())
    }
}

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

    fn to_name(&self, root: &str) -> String {
        let rx = regex::Regex::new(r"^#").unwrap();
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
        let meta = MetaSchema::from_schema(uri.clone(), self.clone())?;
        if schema_map.insert(uri.clone(), meta).is_some() {
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
                &uri.join("not"),
                schema_map,
            )?
        }
        if let Some(SchemaOrSchemas::Schema(ref schema)) = self.items {
            schema.gather_definitions(
                &uri.join("items"),
                schema_map,
            )?
        }
        Ok(())
    }

    fn schema_type(&self) -> Result<SchemaType> {
        use SimpleType::*;
        if let Some(ref_) = self.ref_.as_ref() {
            Ok(SchemaType::Reference(ref_.as_str().into()))
        } else if self.enum_.is_some() {
            Ok(SchemaType::Enum)
        } else if self.any_of.is_some() {
            Ok(SchemaType::AnyOf)
        } else if self.one_of.is_some() {
            Ok(SchemaType::OneOf)
        } else if self.all_of.is_some() {
            Ok(SchemaType::AllOf)
        } else if let Some(ref st) = self.type_ {
            let st = st.unwrap_or_bail()?;
            match st {
                Boolean | Integer | Null | Number | String => Ok(SchemaType::Primitive(st)),
                Array => Ok(SchemaType::Array),
                Object => Ok(SchemaType::Object)
            }
        } else if self.properties.is_some() {
            Ok(SchemaType::Object)
        } else {
            //bail!("Failed to identify schema {}", self)
            Ok(SchemaType::Untyped)  // Default assume object
        }
    }
}

// fn merge_schemas(schemas: &Vec<Schema>, map: &SchemaMap) -> Result<Schema> {
//     let mut all_props = Map::<Schema>::new();
//     let mut types = Vec::new();
//     let _: () = schemas.iter().map(|schema| {
//         let real_schema) = schema.resolve("dummy".into(), map)?;
//         if let Some(ref type_) = real_schema.type_ {
//             types.push(type_.unwrap_or_bail()?)
//         };
//         real_schema.properties.as_ref().map(|map| {
//             map.iter().map(|(name, propschema)| {
//                 match all_props.insert(name.clone(), propschema.clone()) {
//                     Some(_) => bail!("Duplicate key found: {}", name),
//                     None => Ok(())
//                 }
//             }).collect::<Result<Vec<_>>>().map(|_| ())
//         }).unwrap_or(Ok(()))
//     }).collect::<Result<Vec<()>>>().map(|_| ())?;
//     let mut newschema: Schema = Default::default();
//     let type_ = {
//         let dedup: HashSet<_> = types.iter().collect();
//         match dedup.len() {
//             0 => SimpleType::Object,
//             1 => *types.first().unwrap(),
//             _ => bail!("Inconsistent types for allOf: {:?}", types)
//         }
//     };
//     newschema.type_ = Some(SimpleTypeOrSimpleTypes::SimpleType(type_));
//     newschema.properties = if all_props.len() > 0 {
//         Some(all_props)
//     } else {
//         None
//     };
//     Ok(newschema)
// }

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

    fn gather_definitions(&self) -> Result<SchemaMap> {
        let mut map = SchemaMap::new();
        self.0.gather_definitions(&"#".into(), &mut map)?;
        Ok(map)
    }

    fn renderables(&self, root: &str) -> Result<Vec<Renderable>> {
        let map = self.gather_definitions()?;
        let defns = collect_renderable_definitions(&map);
        defns
            .iter()
            .map(|metaschema| metaschema.to_renderable(root, &map))
            .collect()
    }

    pub fn generate(&self, root: &str) -> Result<String> {
        let renderables = self.renderables(root)?;
        let tokens = render_all(&renderables);
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

fn collect_renderable_definitions(map: &SchemaMap) -> Vec<&MetaSchema> {
    // Filter out schemas which are just references to other schemas
    map.values()
        .filter(|metaschema| {
            if metaschema.uri.is_definition() {
                true   // always create regardless of content
            } else {
                use SchemaType::*;
                match metaschema.schema_type {
                    Reference(_) | Primitive(_) | Array | Untyped => false,
                    Enum | AnyOf | AllOf | OneOf | Object => true
                }
            }
        })
        .collect()
}

type Schemas = Vec<Schema>;

enum SchemaType {
    Reference(Uri),
    Primitive(SimpleType),
    Array,
    Enum,
    AllOf,
    AnyOf,
    OneOf,
    Object,
    Untyped,
}

struct MetaSchema {
    uri: Uri,
    schema_type: SchemaType,
    schema: Schema
}

impl MetaSchema {
    fn from_schema(uri: Uri, schema: Schema) -> Result<MetaSchema> {
        let schema_type = schema.schema_type()?;
        Ok(MetaSchema{uri, schema_type, schema})
    }

    fn typename(&self, root: &str, required: bool, map: &SchemaMap) -> Result<TypeName> {
        use SchemaType::*;
        let mods = if !required { vec![Modifier::Option] } else { vec![] };
        match self.schema_type {
            Reference(ref uri) => map.get(uri)
                .ok_or_else(|| format!("Dereference failed for {}", self.uri).into())
                .and_then(|deref| deref.typename(root, required, map)),
            Primitive(st) => Ok(TypeName::new(st.native_typename().unwrap().into(), mods)),
            Array => {
                map.get(&self.uri.join("items"))
                    .map(|metaschema| {
                        metaschema.typename(root, true, map)
                            .map(|mut typename| {
                                typename.modifiers.push(Modifier::Vec);
                                typename.modifiers.extend(&mods);
                                typename
                            })
                    })
                    .unwrap_or(Ok(TypeName::new(GENERIC_TYPE.into(), mods)))
            }
            Untyped => Ok(TypeName::new(GENERIC_TYPE.into(), mods)),
            _ => Ok(TypeName::new(GENERIC_TYPE.into(), mods)),
        }
    }

    fn to_renderable(&self, root: &str, map: &SchemaMap) -> Result<Renderable> {
        println!("Rendering: {}", self.uri);
        use SchemaType::*;
        match self.schema_type {
            Primitive(_) | Reference(_) | Array | Untyped => {
                let name = self.uri.to_name(root);
                let inner = self.typename(root, true, map)?;
                let tags = vec![];
                Ok(Renderable::Alias(Alias::new(name, inner, tags)))
            },
            AnyOf | OneOf => unimplemented!(),
            AllOf => unimplemented!(),
            Enum => unimplemented!(),
            Object => self.renderable_object(root, map)
        }
    }

    fn renderable_object(&self, root: &str, map: &SchemaMap) -> Result<Renderable> {
        // if let Some(anyarr) = self.any_of.as_ref() {
        //     // we are going to make an enum
        //     let variants : Result<Vec<_>> = anyarr.iter().enumerate().map(|(ix, any)| {
        //         let uri = uri.join("anyOf").join(ix);
        //         Ok(any.typename(root, &uri, true, map))
        //     }).collect();
        //     return Ok(Renderable::Enum(Enum::new(name, vec![], variants?)?))
        // }

        // if let Some(allarr) = self.all_of.as_ref() {
        //     println!("ALL OF: {}", uri);
        //     let merged_schema = merge_schemas(allarr, map)?;
        //     return merged_schema.to_renderable(root, &uri, map)
        // }

        let required_keys: HashSet<String> = self.schema.required
            .as_ref()
            .map(|v| v.iter().map(|s| s.clone()).collect())
            .unwrap_or(HashSet::new());
        let fields = self.schema.properties
            .as_ref()
            .unwrap_or(&Map::new())
            .keys()
            .map(|name| {
                let uri = self.uri.join("properties").join(name);
                let metaschema = map.get(&uri)
                    .ok_or(ErrorKind::from(format!("Dereference failed for {}", uri)))?;
                let is_required = required_keys.contains(name);
                let tags = vec![];
                let typename = metaschema.typename(root, is_required, map)?;
                Ok(Field::new(name.clone(), typename, tags)
                   .chain_err(|| format!("Failed to create field {} at uri {}", name, uri))?)
            })
            .collect::<Result<Vec<Field>>>()?;
        let name = self.uri.to_name(root);
        let tags = vec![];
        Ok(Renderable::Struct(Struct::new(name, tags, fields)
                              .chain_err(|| format!("Failed to create struct at uri {}", self.uri))?))
    }
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

// TODO could remove this and just use Box<ToTokens>
#[derive(Clone, PartialEq, Debug)]
enum Renderable {
    Alias(Alias),
    Enum(Enum),
    Struct(Struct),
}

impl ToTokens for Renderable {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use Renderable::*;
        match *self {
            Alias(ref elem) => elem.to_tokens(tokens),
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

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, new)]
struct TypeName {
    base: String,
    modifiers: Vec<Modifier>
}

impl ToTokens for TypeName {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use Modifier::*;
        let base = Ident::new(&*self.base);
        let mut tok = quote!{ #base };
        for modifier in &self.modifiers {
            tok = match *modifier {
                Option => quote! {
                    Option<#tok>
                },
                Box => quote! {
                    Box<#tok>
                },
                Vec => quote! {
                    Vec<#tok>
                }
            };
        }
        tokens.append(tok)
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Deserialize, Serialize)]
enum Modifier {
    Option,
    Box,
    Vec
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
    typename: TypeName,
    tags: Vec<Tokens>,
}

impl Field {
    fn new(name: String, typename: TypeName, mut tags: Vec<Tokens>) -> Result<Field> {
        let snake = make_valid_identifier(name.to_snake_case())?;
        if name != snake {
            tags.push(quote!{
                #[serde(rename = #name)]
            })
        }
        Ok(Field {
            name: snake,
            typename,
            tags,
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags: Vec<_> = self.tags.iter().map(|t| Ident::new(t.as_str())).collect();
        let typename = &self.typename;
        let tok =
            quote! {
            #(#tags),*
            #name: #typename
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

#[derive(Clone, PartialEq, Debug)]
pub struct Alias {
    name: String,
    tags: Vec<Tokens>,
    typename: TypeName,
}

impl Alias {
    fn new(name: String, typename: TypeName, tags: Vec<Tokens>) -> Alias {
        Alias{name, tags, typename}
    }
}

impl ToTokens for Alias {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags: Vec<_> = self.tags.iter().map(|t| Ident::new(t.as_str())).collect();
        let typename = &self.typename;
        let tok =
            quote! {
            #(#tags),*
            type #name = #typename;
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
        let id1: Uri = "#/this/is/some/route/my type".into();
        assert_eq!(id1.to_name("root"), "MyType");
        let id2: Uri = "#/some/other/route/1".into();
        assert_eq!(id2.to_name("root"), "RootSomeOtherRoute1");
        let id3: Uri = "#".into();
        assert_eq!(id3.to_name("root"), "Root");
    }

    #[test]
    fn test_simple_schema() {
        let root = RootSchema::from_file_yaml("test_schemas/simple.yaml").unwrap();
        root.generate("Test").unwrap();
    }
}
