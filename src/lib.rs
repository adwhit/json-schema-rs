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
extern crate simple_codegen;
#[macro_use]
extern crate lazy_static;

use simple_codegen::utils::rust_format;
use simple_codegen::*;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::ops::Deref;

use inflector::Inflector;

use errors::*;
use schema::*;

#[allow(unused_doc_comment)]
mod errors {
    use simple_codegen::errors as codegen;
    error_chain!{
        links {
            Codegen(codegen::Error, codegen::ErrorKind);
        }
        foreign_links {
            Io(::std::io::Error);
            Json(::serde_json::Error);
            Yaml(::serde_yaml::Error);
            Utf8(::std::string::FromUtf8Error);
        }
    }
}

mod schema;

lazy_static! {
    static ref GENERIC_TYPE: Type = Type::named("JsonValue").unwrap();
}

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
        // TODO: combine arr search regexs
        let re_def = regex::Regex::new(r"/definitions/([^/]+)$").unwrap();
        let re_prop = regex::Regex::new(r"/properties/([^/]+)$").unwrap();
        let re_allof = regex::Regex::new(r"/allOf$").unwrap();
        let re_allof_entry = regex::Regex::new(r"/allOf/\d+$").unwrap();
        let re_anyof = regex::Regex::new(r"/anyOf$").unwrap();
        let re_oneof = regex::Regex::new(r"/oneOf$").unwrap();
        if self.deref().find('/').is_none() {
            Identified::Root
        } else if let Some(c) = re_def.captures(&*self) {
            Identified::Definition(c.get(1).unwrap().as_str())
        } else if let Some(c) = re_prop.captures(&*self) {
            return Identified::Property(c.get(1).unwrap().as_str());
        } else if re_allof.is_match(&*self) {
            return Identified::AllOf;
        } else if re_anyof.is_match(&*self) {
            return Identified::AnyOf;
        } else if re_oneof.is_match(&*self) {
            return Identified::OneOf;
        } else if re_allof_entry.is_match(&*self) {
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

    fn to_ident(&self) -> Result<Id> {
        use Identified::*;
        let name = match self.identify() {
            Definition(def) => def.to_pascal_case(),
            Root => self.0.to_pascal_case(),
            Property(prop) => {
                format!(
                    "{}{}",
                    self.strip_right().strip_right().to_ident()?,
                    prop.to_pascal_case()
                )
            }
            AllOfEntry => "XXX This should never be rendered".into(),
            AllOf | AnyOf | OneOf => return self.strip_right().to_ident(),
            Unknown => (&*self).replace("/", " ").to_pascal_case(),
        };
        Ok(Id::valid(name)?)
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Identified<'a> {
    Definition(&'a str),
    Property(&'a str),
    Root,
    AllOf,
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
                        .and_then(|name| {
                            Ok(Variant::new(
                                Id::valid(name.to_string())?,
                                None,
                                vec![],
                            ))
                        })
                })
                .collect::<Result<Vec<Variant>>>()?;
            Enum(variants)
        } else if let Some(BoolOrSchema::Schema(ref schema)) = self.additional_properties {
            let ap_uri = uri.join("additionalProperties");
            schema.identify_and_gather(ap_uri.clone(), root, map)?;
            Map(ap_uri)
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
                ST::Integer => Primitive(PrimEnum::I64),
                ST::Null => Primitive(PrimEnum::Null),
                ST::Number => Primitive(PrimEnum::F64),
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
                        if fields.len() == 0 {
                            Untyped
                        } else {
                            Object { required, fields }
                        }
                    } else {
                        Untyped // Default
                    }
                }
            }
        };
        if map.insert(uri.clone(), meta).is_some() {
            bail!("Uri {} already present in schema map", uri)
        }
        Ok(())
    }
}

fn all_of_to_struct(uris: &[Uri], uri: &Uri, map: &SchemaMap) -> Result<Struct> {
    let structs = uris.iter()
        .map(|field_uri| {
            match mapget(map, &field_uri)?.resolve(map)? {
                &MetaSchema::Object { ref required, ref fields } => {
                    object_to_struct(field_uri, required, fields, map)
                }
                &MetaSchema::AllOf(ref uris) => {
                    let nexturi = uri.join("allOf");
                    all_of_to_struct(uris, &nexturi, map)
                }
                _ => Err(ErrorKind::from("AllOf members must be objects").into())
            }
        })
        .collect::<Result<Vec<Struct>>>()?;
    let name = uri.to_ident()?;
    Ok(Struct::merge(name, Visibility::Public, Attributes::default(), &structs)?)
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

    fn make_items(&self) -> Result<Vec<Box<Item>>> {
        let map = self.gather_definitions()?;
        let defns = collect_definitions_to_render(&map);
        defns
            .iter()
            .map(|&(uri, metaschema)| metaschema.to_item(uri, &map))
            .collect()
    }

    pub fn generate(&self) -> Result<String> {
        let items = self.make_items()?;
        let mut out = String::new();
        for i in items {
            out += &i.to_string();
        }
        Ok(rust_format(&out)?)
    }
}

fn collect_definitions_to_render(map: &SchemaMap) -> Vec<(&Uri, &MetaSchema)> {
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
                    Map(_) | Enum(_) | AnyOf(_) | AllOf(_) | OneOf(_) | Object { .. } => true,
                }
            }
        })
        .collect()
}

#[derive(Clone, PartialEq, Debug)]
enum MetaSchema {
    Reference(Uri),
    Primitive(Primitive),
    Array(Option<Uri>),
    Enum(Vec<Variant>),
    Map(Uri),
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

    fn to_item(&self, uri: &Uri, map: &SchemaMap) -> Result<Box<Item>> {
        use Enum as EnumType;
        use MetaSchema::*;
        match *self {
            Map(_) | Primitive(_) | Reference(_) | Array(_) | Untyped => {
                let name = uri.to_ident()?;
                let inner = typedef_name(uri, self, true, map)?;
                Ok(Box::new(Alias::new(name, Visibility::Public, inner)))
            }
            AnyOf(ref uris) | OneOf(ref uris) => {
                unimplemented!()
                // let name = uri.to_ident()?;
                // let variants = uris.iter()
                //     .map(|uri| {
                //         mapget(map, &uri).and_then(|elem| {
                //             typedef_name(uri, elem, true, map).and_then(|typename| {
                //                 let name = make_valid_identifier(&typename.apply_modifiers())?
                //                     .into_owned();
                //                 Ok(Variant::new(name, vec![], Some(typename))?)
                //             })
                //         })
                //     })
                //     .collect::<Result<Vec<_>>>()?;
                // Ok(EnumType::new(name, vec![], variants)?)
            }
            AllOf(ref uris) => {
                let uri = uri.join("allOf");
                all_of_to_struct(uris, &uri, map)
                    .chain_err(|| format!("Failed to render {}", uri))
                    .map(|s| Box::new(s) as Box<Item>)
            }
            Enum(ref variants) => {
                let name = uri.to_ident()?;
                Ok(Box::new(EnumType::new(
                    name,
                    Visibility::Public,
                    Attributes::default(),
                    variants.clone(),
                )))
            }
            Object {
                ref required,
                ref fields,
            } => object_to_struct(uri, required, fields, map).map(|s| Box::new(s) as Box<Item>)
        }
    }
}

fn object_to_struct(
    uri: &Uri,
    required_keys: &HashSet<String>,
    field_map: &Map<Uri>,
    map: &SchemaMap,
) -> Result<Struct> {
    let name = uri.to_ident()?;
    let fields = field_map
        .iter()
        .map(|(field_name, uri)| {
            let metaschema = mapget(map, &uri)?;
            let is_required = required_keys.contains(field_name);
            let typ = typedef_name(uri, metaschema, is_required, map)?;
            Ok(Field::with_rename(field_name.as_str(), typ)?)
        })
        .collect::<Result<Vec<Field>>>()?;
    Ok(Struct::new(
        name,
        Visibility::Public,
        Attributes::default(),
        fields,
    ))
}

fn mapget<'a>(map: &'a SchemaMap, uri: &'a Uri) -> Result<&'a MetaSchema> {
    map.get(uri).ok_or_else(|| {
        format!("Dereference failed for {}", uri).into()
    })
}

/// Fetch the name of the type referred to by the metaschema
/// e.g. for "type MyVec = Vec<Data>", 'Vec<Data>' is the typedef name.
/// The name will be inferred from the uri if necessary
fn typedef_name(uri: &Uri, meta: &MetaSchema, required: bool, map: &SchemaMap) -> Result<Type> {
    use MetaSchema::*;
    match *meta {
        Reference(ref uri) => {
            mapget(map, uri).and_then(|deref| typedef_name(uri, deref, required, map))
        }
        Primitive(prim) => Ok(Type::Primitive(prim).optional(!required)),
        Array(Some(ref items_uri)) => {
            let meta = mapget(map, items_uri)?;
            Ok(Type::Vec(
                Box::new(typedef_name(items_uri, meta, required, map)?),
            ))
        }
        Map(ref items_uri) => {
            let meta = mapget(map, items_uri)?;
            Ok(Type::Map(
                Box::new(typedef_name(items_uri, meta, required, map)?),
            ))
        }
        Array(None) => Ok(Type::Vec(Box::new(GENERIC_TYPE.clone())).optional(
            !required,
        )),
        Untyped => Ok(GENERIC_TYPE.clone().optional(!required)),
        Object { .. } | _ => Ok(Type::Named(uri.to_ident()?).optional(!required)),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uri_to_ident() {
        let id1 = Uri::new("root", "#/this/is/some/route/my type");
        assert_eq!(id1.to_ident().unwrap().deref(), "RootThisIsSomeRouteMyType");
        let id2 = Uri::new("root", "#/properties/aProp");
        assert_eq!(id2.to_ident().unwrap().deref(), "RootAprop");
        let id3 = Uri::new("root", "#");
        assert_eq!(id3.to_ident().unwrap().deref(), "Root");
        // TODO make this work
        // let id4: Uri = "#more".into();
        // assert_eq!(id3.to_name("root"), "More");
    }

    #[test]
    fn test_simple_schema() {
        let root = RootSchema::from_file_yaml("test simple".into(), "test_schemas/simple.yaml")
            .unwrap();
        let out = root.generate().unwrap();
        println!("{}", out);
    }

    #[test]
    fn test_meta_schema() {
        let root = RootSchema::from_file_yaml("schema".into(), "test_schemas/metaschema.json")
            .unwrap();
        let out = root.generate().unwrap();
        println!("{}", out);
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
