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
use schema::*;

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
mod schema;

lazy_static! {
    static ref RUST_KEYWORDS: HashSet<&'static str> = {
        keywords::RUST_KEYWORDS.iter().map(|v| *v).collect()
    };
}

const GENERIC_TYPE: &str = "JsonValue";
const HEADER: &str = "use ::serde_json::Value as JsonValue;";

type SchemaMap = BTreeMap<Uri, MetaSchema>;

impl fmt::Display for Schema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", serde_json::to_string_pretty(self).unwrap())
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Deserialize, Serialize)]
struct Uri(String);

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

impl Uri {
    fn new(root: &str, path: &str) -> Uri {
        if path == "#" {
            Uri(root.to_string())
        } else if path.starts_with("#/") {
            let (_, right) = path.split_at(2);
            Uri(format!("{}/{}", root, right))
        } else {
            Uri(path.into())
        }
    }

    fn identify(&self) -> Identified {
        let re_def = regex::Regex::new(r"/definitions/([^/]+)$").unwrap();
        let re_prop = regex::Regex::new(r"/properties/([^/]+)$").unwrap();
        let re_allof = regex::Regex::new(r"/allOf/\d+$").unwrap();
        let re_anyof = regex::Regex::new(r"/anyOf$").unwrap();
        let re_oneof = regex::Regex::new(r"/oneOf$").unwrap();
        if self.deref().find('/').is_none() {
            Identified::Root
        } else if let Some(c) = re_def.captures(&*self) {
            Identified::Definition(c.get(1).unwrap().as_str())
        } else if let Some(c) = re_prop.captures(&*self) {
            return Identified::Property(c.get(1).unwrap().as_str());
        } else if re_anyof.is_match(&*self) {
            return Identified::AnyOf;
        } else if re_oneof.is_match(&*self) {
            return Identified::OneOf;
        } else if re_allof.is_match(&*self) {
            return Identified::AllOfEntry;
        } else {
            Identified::Unknown
        }
    }

    fn is_definition(&self) -> bool {
        if let Identified::Definition(_) = self.identify() {
            true
        } else {
            false
        }
    }

    fn is_all_of_entry(&self) -> bool {
        Identified::AllOfEntry == self.identify()
    }

    fn join<T: fmt::Display>(&self, next: T) -> Uri {
        Uri(format!("{}/{}", self, next))
    }

    fn strip_right(&self) -> Uri {
        if let Some(n) = self.0.rfind('/') {
            let (left, _) = self.0.split_at(n);
            Uri(left.into())
        } else {
            self.clone()
        }
    }

    fn to_type_name(&self) -> Result<String> {
        use Identified::*;
        let name = match self.identify() {
            Definition(def) => def.to_pascal_case(),
            Root => self.0.to_pascal_case(),
            Property(prop) => {
                format!(
                    "{}{}",
                    self.strip_right().strip_right().to_type_name()?,
                    prop.to_pascal_case()
                )
            }
            AllOfEntry => "XXX This should never be rendered".into(),
            AnyOf | OneOf => return self.strip_right().to_type_name(),
            Unknown => (&*self).replace("/", " ").to_pascal_case(),
        };
        make_valid_identifier(&name)
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Identified<'a> {
    Definition(&'a str),
    Property(&'a str),
    Root,
    AllOfEntry,
    AnyOf,
    OneOf,
    Unknown,
}

impl Schema {
    fn identify_and_gather(&self, uri: Uri, root: &str, map: &mut SchemaMap) -> Result<()> {
        if let Some(defns) = self.definitions.as_ref() {
            gather_definitions_map(&defns, &uri.join("definitions"), root, map)?;
        }
        use MetaSchema::*;
        let meta = if let Some(ref_) = self.ref_.as_ref() {
            Reference(Uri::new(root, ref_))
        } else if let Some(ref enums) = self.enum_ {
            let variants = enums
                .iter()
                .map(|enm| {
                    enm.as_str()
                        .ok_or(ErrorKind::from("enum items must be strings").into())
                        .map(|name| Variant::new(name.to_string(), None))
                })
                .collect::<Result<Vec<Variant>>>()?;
            Enum(variants)
        } else if let Some(ref schemas) = self.any_of {
            let uris = gather_definitions_vec(schemas, &uri.join("anyOf"), root, map)?;
            AnyOf(uris)
        } else if let Some(ref schemas) = self.one_of {
            let uris = gather_definitions_vec(schemas, &uri.join("oneOf"), root, map)?;
            OneOf(uris)
        } else if let Some(ref schemas) = self.all_of {
            let uris = gather_definitions_vec(schemas, &uri.join("allOf"), root, map)?;
            AllOf(uris)
        } else {
            use SimpleType as ST;
            use Primitive as PrimEnum;
            let st = self.type_
                .as_ref()
                .map(|st| {
                    st.unwrap_or_bail().chain_err(|| {
                        format!("Failed to get schema type for {:?}", st)
                    })
                })
                .unwrap_or(Ok(ST::Object))?;
            match st {
                ST::Boolean => Primitive(PrimEnum::Boolean),
                ST::Integer => Primitive(PrimEnum::Integer),
                ST::Null => Primitive(PrimEnum::Null),
                ST::Number => Primitive(PrimEnum::Number),
                ST::String => Primitive(PrimEnum::String),
                ST::Array => {
                    let uri = self.items
                        .as_ref()
                        .map(|items| match *items {
                            SchemaItems::Schema(ref schema) => {
                                let uri = uri.join("items");
                                schema.identify_and_gather(uri.clone(), root, map)?;
                                Ok(Some(uri))
                            }
                            SchemaItems::Schemas(_) => Err(ErrorKind::from(
                                "Multiple array items not supported",
                            )),
                        })
                        .unwrap_or(Ok(None))?;
                    Array(uri)
                }
                ST::Object => {
                    if let Some(ref props) = self.properties {
                        let required: HashSet<String> = self.required
                            .as_ref()
                            .map(|v| v.iter().map(|s| s.clone()).collect())
                            .unwrap_or(HashSet::new());
                        let fields =
                            gather_definitions_map(&props, &uri.join("properties"), root, map)?;
                        Object { required, fields }
                    } else {
                        Untyped // Default
                    }
                }
            }
        };
        println!("URI: {}", uri);
        if map.insert(uri.clone(), meta).is_some() {
            bail!("Uri {} already present in schema map", uri)
        }
        Ok(())
    }
}

fn render_all_of(uris: &[Uri], uri: &Uri, map: &SchemaMap) -> Result<Renderable> {
    let mut fields = Vec::new();
    uris.iter()
        .map(|uri| {
            mapget(map, &uri)?
                .resolve(map)?
                .to_renderable(&uri, map)
                .map(|renderable| match renderable {
                    Renderable::Struct(struct_) => {
                        fields.extend(struct_.fields);
                        Ok(())
                    }
                    _ => Err(ErrorKind::from("AnyOf members must be objects")),
                })
        })
        .collect::<Result<Vec<_>>>()?;
    {
        let mut fieldcheck = HashSet::new();
        fields
            .iter()
            .map(|field| if !fieldcheck.insert(&field.name) {
                bail!("anyOf duplicate field '{}' for {}", field.name, uri)
            } else {
                Ok(())
            })
            .collect::<Result<Vec<_>>>()?;
    }
    let name = uri.to_type_name()?;
    let tags = vec![];
    Ok(Renderable::Struct(Struct::new(name, tags, fields)))
}

fn gather_definitions_map(
    schema_map: &Map<Schema>,
    uri: &Uri,
    root: &str,
    map: &mut SchemaMap,
) -> Result<Map<Uri>> {
    schema_map
        .iter()
        .map(|(name, schema)| {
            let uri = uri.join(name);
            schema.identify_and_gather(uri.clone(), root, map)?;
            Ok((name.clone(), uri))
        })
        .collect::<Result<BTreeMap<String, Uri>>>()
}

fn gather_definitions_vec(
    schemas: &SchemaArray,
    uri: &Uri,
    root: &str,
    map: &mut SchemaMap,
) -> Result<Vec<Uri>> {
    schemas
        .iter()
        .enumerate()
        .map(|(ix, schema)| {
            let uri = uri.join(ix + 1);
            schema.identify_and_gather(uri.clone(), root, map)?;
            Ok(uri)
        })
        .collect::<Result<Vec<_>>>()
}

#[derive(Clone, PartialEq, Debug, Default, Deserialize, Serialize)]
pub struct RootSchema {
    name: String,
    schema: Schema,
}

impl RootSchema {
    pub fn from_reader_yaml<R: Read>(name: String, reader: R) -> Result<RootSchema> {
        let schema: Schema = serde_yaml::from_reader(reader)?;
        Ok(RootSchema { name, schema })
    }

    pub fn from_file_yaml<P: AsRef<Path>>(name: String, path: P) -> Result<RootSchema> {
        Ok(RootSchema::from_reader_yaml(name, File::open(path)?)?)
    }

    pub fn from_reader_json<R: Read>(name: String, reader: R) -> Result<RootSchema> {
        let schema = serde_json::from_reader(reader)?;
        Ok(RootSchema { name, schema })
    }

    pub fn from_file_json<P: AsRef<Path>>(name: String, path: P) -> Result<RootSchema> {
        Ok(RootSchema::from_reader_json(name, File::open(path)?)?)
    }

    fn gather_definitions(&self) -> Result<SchemaMap> {
        let mut map = SchemaMap::new();
        let uri = Uri::new(&self.name, "#");
        self.schema.identify_and_gather(uri, &self.name, &mut map)?;
        Ok(map)
    }

    fn make_renderables(&self) -> Result<Vec<Renderable>> {
        let map = self.gather_definitions()?;
        let defns = collect_renderable_definitions(&map);
        defns
            .iter()
            .map(|&(uri, metaschema)| metaschema.to_renderable(uri, &map))
            .collect()
    }

    pub fn generate(&self) -> Result<String> {
        let renderables = self.make_renderables()?;
        let tokens = render_all(&renderables);
        rust_format(tokens)
    }
}

fn render_all(renderables: &[Renderable]) -> Tokens {
    renderables.iter().fold(
        Tokens::new(),
        |mut tokens, renderable| {
            renderable.to_tokens(&mut tokens);
            tokens
        },
    )
}

fn collect_renderable_definitions(map: &SchemaMap) -> Vec<(&Uri, &MetaSchema)> {
    // Filter out schemas which are just references to other schemas
    use MetaSchema::*;
    map.iter()
        .filter(|&(uri, metaschema)| {
            if uri.is_definition() {
                true // always create regardless of content
            } else if uri.is_all_of_entry() {
                false // 'partial' schema
            } else {
                match *metaschema {
                    Reference(_) | Primitive(_) | Array(_) | Untyped => false,
                    Enum(_) | AnyOf(_) | AllOf(_) | OneOf(_) | Object { .. } => true,
                }
            }
        })
        .collect()
}

#[derive(Clone, Copy, PartialEq, Debug, Deserialize, Serialize)]
enum Primitive {
    Null,
    Boolean,
    Integer,
    Number,
    String,
}

impl Primitive {
    fn native(&self) -> &str {
        use Primitive::*;
        match *self {
            Null => "()",
            Boolean => "bool",
            Integer => "i64",
            Number => "f64",
            String => "String",
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum MetaSchema {
    Reference(Uri),
    Primitive(Primitive),
    Array(Option<Uri>),
    Enum(Vec<Variant>),
    AllOf(Vec<Uri>),
    AnyOf(Vec<Uri>),
    OneOf(Vec<Uri>),
    Object {
        required: HashSet<String>,
        fields: Map<Uri>,
    },
    Untyped,
}

impl MetaSchema {
    fn resolve<'a>(&'a self, map: &'a SchemaMap) -> Result<&'a MetaSchema> {
        if let MetaSchema::Reference(ref uri) = *self {
            mapget(map, uri).and_then(|schema| schema.resolve(map))
        } else {
            Ok(self)
        }
    }

    fn to_renderable(&self, uri: &Uri, map: &SchemaMap) -> Result<Renderable> {
        use Enum as EnumType;
        use MetaSchema::*;
        match *self {
            Primitive(_) | Reference(_) | Array(_) | Untyped => {
                let name = uri.to_type_name()?;
                let inner = typedef_name(uri, self, true, map)?;
                let tags = vec![];
                Ok(Renderable::Alias(Alias::new(name, inner, tags)))
            }
            AnyOf(ref uris) | OneOf(ref uris) => {
                let name = uri.to_type_name()?;
                let variants = uris.iter()
                    .map(|uri| {
                        mapget(map, &uri).and_then(|elem| {
                            let name = uri.to_type_name()?;
                            typedef_name(uri, elem, true, map).map(|typename| {
                                Variant::new(name.to_string(), Some(typename))
                            })
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Renderable::Enum(EnumType::new(name, vec![], variants)?))
            }
            AllOf(ref uris) => {
                let uri = uri.join("allOf");
                render_all_of(uris, &uri, map)
            }
            Enum(ref variants) => {
                let name = uri.to_type_name()?;
                Ok(Renderable::Enum(EnumType::new(name, vec![], variants.clone())?))
            }
            Object {
                ref required,
                ref fields,
            } => renderable_object(uri, required, fields, map),
        }
    }
}

fn renderable_object(
    uri: &Uri,
    required_keys: &HashSet<String>,
    field_map: &Map<Uri>,
    map: &SchemaMap,
) -> Result<Renderable> {
    let fields = field_map
        .iter()
        .map(|(field_name, uri)| {
            let metaschema = mapget(map, &uri)?;
            let is_required = required_keys.contains(field_name);
            let tags = vec![];
            let typename = typedef_name(uri, metaschema, is_required, map)?;
            Ok(Field::new(field_name.clone(), typename, tags).chain_err(
                || {
                    format!("Failed to create field {} at uri {}", field_name, uri)
                },
            )?)
        })
        .collect::<Result<Vec<Field>>>()?;
    let name = uri.to_type_name()?;
    let tags = vec![];
    Ok(Renderable::Struct(Struct::new(name, tags, fields)))
}

fn mapget<'a>(map: &'a SchemaMap, uri: &'a Uri) -> Result<&'a MetaSchema> {
    map.get(uri).ok_or_else(|| {
        format!("Dereference failed for {}", uri).into()
    })
}

fn typedef_name(uri: &Uri, meta: &MetaSchema, required: bool, map: &SchemaMap) -> Result<TypeName> {
    let mut mods = Vec::new();
    if !required {
        mods.push(Modifier::Option)
    };
    use MetaSchema::*;
    match *meta {
        Reference(ref uri) => {
            mapget(map, uri).and_then(|deref| typedef_name(uri, deref, required, map))
        }
        Primitive(prim) => Ok(TypeName::new(prim.native().into(), mods)),
        Array(Some(ref items_uri)) => {
            mods.insert(0, Modifier::Vec);
            let meta = mapget(map, items_uri)?;
            typedef_name(items_uri, meta, true, map).map(|mut typename| {
                typename.modifiers.extend(&mods);
                typename
            })
        }
        Array(None) => {
            mods.insert(0, Modifier::Vec);
            Ok(TypeName::new(GENERIC_TYPE.into(), mods))
        }
        Untyped => Ok(TypeName::new(GENERIC_TYPE.into(), mods)),
        Object { .. } | _ => Ok(TypeName::new(uri.to_type_name()?, mods)),
    }
}

impl SchemaType {
    fn unwrap_or_bail(&self) -> Result<SimpleType> {
        match *self {
            SchemaType::SimpleType(st) => Ok(st),
            _ => bail!("Multiple types not supported"),
        }
    }
}

fn make_valid_identifier(s: &str) -> Result<String> {
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
struct Variant {
    name: Ident,
    type_: Option<TypeName>,
}

impl Variant {
    fn new(name: String, type_: Option<TypeName>) -> Variant {
        Variant { name: Ident::from(name), type_ }
    }
}

impl ToTokens for Variant {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tok = if let Some(ref type_) = self.type_ {
            quote! {
                #name(#type_)
            }
        } else {
            quote! {
                #name
            }
        };
        tokens.append(tok);
    }
}

// TODO could remove this and just use Box<ToTokens>
#[derive(Clone, PartialEq, Debug)]
enum Renderable {
    Alias(Alias),
    Struct(Struct),
    Enum(Enum),
}

impl ToTokens for Renderable {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use Renderable::*;
        match *self {
            Alias(ref elem) => elem.to_tokens(tokens),
            Struct(ref elem) => elem.to_tokens(tokens),
            Enum(ref elem) => elem.to_tokens(tokens),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, new)]
struct TypeName {
    base: String,
    modifiers: Vec<Modifier>,
}

impl TypeName {
    fn apply_modifiers(&self) -> String {
        use Modifier::*;
        let mut base = self.base.clone();
        for modifier in &self.modifiers {
            base = match *modifier {
                Option => format!("Option<{}>", base),
                Box => format!("Box<{}>", base),
                Vec => format!("Vec<{}>", base),
            };
        }
        base
    }
}

impl ToTokens for TypeName {
    fn to_tokens(&self, tokens: &mut Tokens) {
        tokens.append(Ident::new(self.apply_modifiers()))
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Deserialize, Serialize)]
enum Modifier {
    Option,
    Box,
    Vec,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    name: Ident,
    tags: Vec<Tokens>,
    fields: Vec<Field>,
}

impl Struct {
    fn new(name: String, tags: Vec<String>, fields: Vec<Field>) -> Struct {
        let name = Ident::from(name.to_pascal_case());
        let mut tags = tags.into_iter()
            .map(|s| {
                let s = Ident::from(s);
                quote! {#s}
            })
            .collect::<Vec<_>>();
        tags.push(quote! {
            #[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
        });
        Struct { name, tags, fields }
    }
}

impl ToTokens for Struct {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let fields = &self.fields;
        let tags = &self.tags;
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
    name: Ident,
    typename: TypeName,
    tags: Vec<Tokens>,
}

impl Field {
    fn new(name: String, typename: TypeName, tags: Vec<String>) -> Result<Field> {
        let mut tags = tags.into_iter()
            .map(|s| {
                let s = Ident::from(s);
                quote! {#s}
            })
            .collect::<Vec<_>>();
        let snake = make_valid_identifier(&name.to_snake_case())?;
        if name != snake {
            tags.push(quote! {
                #[serde(rename = #name)]
            })
        }
        Ok(Field {
            name: Ident::from(snake),
            typename,
            tags,
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tags = &self.tags;
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
    name: Ident,
    tags: Vec<Tokens>,
    variants: Vec<Variant>,
}

impl Enum {
    fn new(name: String, tags: Vec<String>, variants: Vec<Variant>) -> Result<Enum> {
        let name = Ident::from(name);
        let tags = tags.into_iter()
            .map(|s| {
                let s = Ident::from(s);
                quote! {#s}
            })
            .collect();
        Ok(Enum {
            name,
            tags,
            variants,
        })
    }
}

impl ToTokens for Enum {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tags = &self.tags;
        let variants = &self.variants;
        let tok =
            quote! {
                #(#tags),*
                pub enum #name {
                    #(#variants),*
                }
            };
        tokens.append(tok);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Alias {
    name: Ident,
    tags: Vec<Tokens>,
    typename: TypeName,
}

impl Alias {
    fn new(name: String, typename: TypeName, tags: Vec<String>) -> Alias {
        let tags = tags.into_iter()
            .map(|s| {
                let s = Ident::from(s);
                quote! {#s}
            })
            .collect();
        Alias {
            name: Ident::from(name),
            tags,
            typename,
        }
    }
}

impl ToTokens for Alias {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tags = &self.tags;
        let typename = &self.typename;
        let tok =
            quote! {
            #(#tags),*
            type #name = #typename;
        };
        tokens.append(tok);
    }
}

fn rust_format(t: Tokens) -> Result<String> {
    use rustfmt::*;

    // FIXME workaround is necessary until rustfmt works programmatically
    //let tmppath = "/tmp/rustfmt.rs"; // TODO use tempdir
    let tmppath = "/home/alex/scratch/stubgen/src/gen.rs"; // TODO use tempdir
    {
        let mut tmp = File::create(tmppath)?;
        writeln!(tmp, "{}", HEADER)?;
        tmp.write_all(t.as_str().as_bytes())?;
    }

    let input = Input::File(tmppath.into());
    let mut fakebuf = Vec::new(); // pretty weird that this is necessary.. but it is

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
    fn test_uri_to_type_name() {
        let id1 = Uri::new("root", "#/this/is/some/route/my type");
        assert_eq!(id1.to_type_name().unwrap(), "RootThisIsSomeRouteMyType");
        let id2 = Uri::new("root", "#/properties/aProp");
        assert_eq!(id2.to_type_name().unwrap(), "RootAprop");
        let id3 = Uri::new("root", "#");
        assert_eq!(id3.to_type_name().unwrap(), "Root");
        // TODO make this work
        // let id4: Uri = "#more".into();
        // assert_eq!(id3.to_name("root"), "More");
    }

    #[test]
    fn test_simple_schema() {
        let root = RootSchema::from_file_yaml("test simple".into(), "test_schemas/simple.yaml")
            .unwrap();
        root.generate().unwrap();
    }

    #[test]
    fn test_meta_schema() {
        let root = RootSchema::from_file_yaml("schema".into(), "test_schemas/metaschema.json")
            .unwrap();
        root.generate().unwrap();
    }

    #[test]
    fn test_debug_server_schema() {
        let root = RootSchema::from_file_yaml(
            "debug server".into(),
            "test_schemas/debugserver-schema.json",
        ).unwrap();
        root.generate().unwrap();
    }
}
