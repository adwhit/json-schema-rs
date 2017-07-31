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
    fn identify(&self) -> Identified {
        let re_def = regex::Regex::new(r"/definitions/([^/]+)$").unwrap();
        let re_prop = regex::Regex::new(r"/properties/([^/]+)$").unwrap();
        let re_allof = regex::Regex::new(r"/allOf/\d+$").unwrap();
        let re_anyof = regex::Regex::new(r"/anyOf$").unwrap();
        let re_oneof = regex::Regex::new(r"/oneOf$").unwrap();
        if self.deref() == "#" {
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
            Uri::from(left)
        } else {
            self.clone()
        }
    }

    fn to_type_name(&self, root: &str) -> Result<String> {
        use Identified::*;
        let name = match self.identify() {
            Definition(def) => def.to_pascal_case(),
            Property(prop) => format!("{}Type", prop.to_pascal_case()),
            Root => root.to_pascal_case(),
            AllOfEntry => "XXX This should never be rendered".into(),
            AnyOf | OneOf => return self.strip_right().to_type_name(root),
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
    /// find every instance in which a schema is defined or referenced
    fn gather_definitions(&self, uri: &Uri, schema_map: &mut SchemaMap) -> Result<()> {
        // TODO: This is a full recursive clone. Would be nice to replace
        // all Schema instances with URIs
        let meta = MetaSchema::from_schema(uri.clone(), self.clone())?;
        if schema_map.insert(uri.clone(), meta).is_some() {
            bail!("Schema already exists at location {}", uri)
        };
        gather_definitions_map(&self.definitions, uri.join("definitions"), schema_map)?;
        gather_definitions_map(&self.properties, uri.join("properties"), schema_map)?;
        gather_definitions_map(
            &self.pattern_properties,
            uri.join("patternProperties"),
            schema_map,
        )?;
        gather_definitions_vec(&self.all_of, uri.join("allOf"), schema_map)?;
        gather_definitions_vec(&self.any_of, uri.join("anyOf"), schema_map)?;
        gather_definitions_vec(&self.one_of, uri.join("oneOf"), schema_map)?;
        if let Some(ref schema) = self.not {
            schema.gather_definitions(&uri.join("not"), schema_map)?
        }
        if let Some(SchemaOrSchemas::Schema(ref schema)) = self.items {
            schema.gather_definitions(&uri.join("items"), schema_map)?
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
                Object => Ok(SchemaType::Object),
            }
        } else if self.properties.is_some() {
            Ok(SchemaType::Object)
        } else {
            Ok(SchemaType::Untyped) // Default assume object
        }
    }
}

fn any_of(arrlen: usize, root: &str, uri: &Uri, map: &SchemaMap) -> Result<Renderable> {
    let mut fields = Vec::new();
    (0..arrlen)
        .map(|ix| uri.join(ix + 1))
        .map(|uri| {
            mapget(map, &uri)?
                .resolve(map)?
                .to_renderable(root, map)
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
    let name = uri.to_type_name(root)?;
    let tags = vec![];
    Ok(Renderable::Struct(Struct::new(name, tags, fields)))
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
            schema.gather_definitions(&uri.join(ix + 1), schema_map)?;
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
                true // always create regardless of content
            } else if metaschema.uri.is_all_of_entry() {
                false //  not a complete schema
            } else {
                use SchemaType::*;
                match metaschema.schema_type {
                    Reference(_) | Primitive(_) | Array | Untyped => false,
                    Enum | AnyOf | AllOf | OneOf | Object => true,
                }
            }
        })
        .collect()
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
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

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
struct MetaSchema {
    uri: Uri,
    schema_type: SchemaType,
    schema: Schema,
}

impl MetaSchema {
    fn from_schema(uri: Uri, schema: Schema) -> Result<MetaSchema> {
        let schema_type = schema.schema_type()?;
        Ok(MetaSchema {
            uri,
            schema_type,
            schema,
        })
    }

    fn typename(&self, root: &str, required: bool, map: &SchemaMap) -> Result<TypeName> {
        use SchemaType::*;
        let mut mods = if !required {
            vec![Modifier::Option]
        } else {
            vec![]
        };
        match self.schema_type {
            Reference(ref uri) => {
                mapget(map, uri).and_then(|deref| deref.typename(root, required, map))
            }
            Primitive(st) => Ok(TypeName::new(st.native_typename().unwrap().into(), mods)),
            Array => {
                mods.insert(0, Modifier::Vec);
                mapget(map, &self.uri.join("items"))
                    .map(|metaschema| {
                        metaschema.typename(root, true, map).map(|mut typename| {
                            typename.modifiers.extend(&mods);
                            typename
                        })
                    })
                    .unwrap_or(Ok(TypeName::new(GENERIC_TYPE.into(), mods)))
            }
            Object => Ok(TypeName::new(self.uri.to_type_name(root)?, mods)),
            Untyped => Ok(TypeName::new(GENERIC_TYPE.into(), mods)),
            _ => Ok(TypeName::new(self.uri.to_type_name(root)?, mods)),
        }
    }

    fn resolve<'a>(&'a self, map: &'a SchemaMap) -> Result<&'a MetaSchema> {
        if let SchemaType::Reference(ref uri) = self.schema_type {
            mapget(map, uri).and_then(|schema| schema.resolve(map))
        } else {
            Ok(self)
        }
    }

    fn to_renderable(&self, root: &str, map: &SchemaMap) -> Result<Renderable> {

        fn any_or_one_of(
            arrlen: usize,
            root: &str,
            uri: &Uri,
            map: &SchemaMap,
        ) -> Result<Renderable> {
            // we are going to make an union
            let name = uri.to_type_name(root)?;
            let variants = (0..arrlen)
                .map(|ix| {
                    let uri = uri.join(ix + 1);
                    mapget(map, &uri).and_then(|elem| Ok(elem.typename(root, true, map)?))
                })
                .collect::<Result<Vec<_>>>()?;
            return Ok(Renderable::Union(Union::new(name, vec![], variants)?));
        }

        use Enum as EnumType;
        use SchemaType::*;
        match self.schema_type {
            Primitive(_) | Reference(_) | Array | Untyped => {
                let name = self.uri.to_type_name(root)?;
                let inner = self.typename(root, true, map)?;
                let tags = vec![];
                Ok(Renderable::Alias(Alias::new(name, inner, tags)))
            }
            AnyOf => {
                let arrlen = self.schema.any_of.as_ref().unwrap().len();
                let uri = self.uri.join("anyOf");
                any_or_one_of(arrlen, root, &uri, map)
            }
            OneOf => {
                let arrlen = self.schema.one_of.as_ref().unwrap().len();
                let uri = self.uri.join("oneOf");
                any_or_one_of(arrlen, root, &uri, map)
            }
            AllOf => {
                let arrlen = self.schema.all_of.as_ref().unwrap().len();
                let uri = self.uri.join("allOf");
                any_of(arrlen, root, &uri, map)
            }
            Enum => {
                let enumvals = self.schema.enum_.as_ref().unwrap();
                let variants = enumvals
                    .iter()
                    .map(|val| {
                        val.as_str().map(|s| s.to_string()).ok_or_else(|| {
                            "Non-string enumerations not supported".into()
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                let name = self.uri.to_type_name(root)?;
                Ok(Renderable::Enum(EnumType::new(name, vec![], variants)?))

            }
            Object => self.renderable_object(root, map),
        }
    }

    fn renderable_object(&self, root: &str, map: &SchemaMap) -> Result<Renderable> {
        let required_keys: HashSet<String> = self.schema
            .required
            .as_ref()
            .map(|v| v.iter().map(|s| s.clone()).collect())
            .unwrap_or(HashSet::new());
        let fields = self.schema
            .properties
            .as_ref()
            .unwrap_or(&Map::new())
            .keys()
            .map(|field_name| {
                let uri = self.uri.join("properties").join(field_name);
                let metaschema = mapget(map, &uri)?;
                let is_required = required_keys.contains(field_name);
                let tags = vec![];
                let typename = metaschema.typename(root, is_required, map)?;
                Ok(Field::new(field_name.clone(), typename, tags).chain_err(
                    || {
                        format!("Failed to create field {} at uri {}", field_name, uri)
                    },
                )?)
            })
            .collect::<Result<Vec<Field>>>()?;
        let name = self.uri.to_type_name(root)?;
        let tags = vec![];
        Ok(Renderable::Struct(Struct::new(name, tags, fields)))
    }
}

fn mapget<'a>(map: &'a SchemaMap, uri: &'a Uri) -> Result<&'a MetaSchema> {
    map.get(uri).ok_or_else(|| {
        format!("Dereference failed for {}", uri).into()
    })
}

impl SimpleTypeOrSimpleTypes {
    fn unwrap_or_bail(&self) -> Result<SimpleType> {
        match *self {
            SimpleTypeOrSimpleTypes::SimpleType(st) => Ok(st),
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

// TODO could remove this and just use Box<ToTokens>
#[derive(Clone, PartialEq, Debug)]
enum Renderable {
    Alias(Alias),
    Union(Union),
    Struct(Struct),
    Enum(Enum),
}

impl ToTokens for Renderable {
    fn to_tokens(&self, tokens: &mut Tokens) {
        use Renderable::*;
        match *self {
            Alias(ref elem) => elem.to_tokens(tokens),
            Union(ref elem) => elem.to_tokens(tokens),
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
    name: String,
    tags: Vec<Tokens>,
    fields: Vec<Field>,
}

impl Struct {
    fn new(name: String, mut tags: Vec<Tokens>, fields: Vec<Field>) -> Struct {
        let name = name.to_pascal_case();
        tags.push(quote! {
            #[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
        });
        Struct { name, tags, fields }
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
        let snake = make_valid_identifier(&name.to_snake_case())?;
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
struct Union {
    name: String,
    tags: Vec<Tokens>,
    variants: Vec<(Ident, TypeName)>,
}

impl Union {
    fn new(name: String, tags: Vec<Tokens>, variants: Vec<TypeName>) -> Result<Union> {
        let variants = variants
            .into_iter()
            .map(|right| {
                let left = make_valid_identifier(&right.apply_modifiers().to_pascal_case())?;
                Ok((Ident::from(left), right))
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Union {
            name,
            tags,
            variants,
        })
    }
}

impl ToTokens for Union {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::new(&*self.name);
        let tags = &self.tags;
        let names = self.variants.iter().map(|v| &v.0);
        let types = self.variants.iter().map(|v| &v.1);
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
        Alias {
            name,
            tags,
            typename,
        }
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

#[derive(Clone, PartialEq, Debug)]
struct Enum {
    name: String,
    tags: Vec<Tokens>,
    variants: Vec<(Ident, Vec<Tokens>)>,
}

impl Enum {
    fn new(name: String, tags: Vec<Tokens>, variants: Vec<String>) -> Result<Enum> {
        let variants = variants
            .iter()
            .map(|variant| {
                let mut tags = Vec::new();
                let validname = make_valid_identifier(&variant.to_pascal_case())?;
                if *variant != validname {
                    tags.push(quote!{
                        #[serde(rename = #variant)]
                    });
                }
                Ok((Ident::from(validname), tags))
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Enum {
            name,
            tags,
            variants,
        })
    }
}

impl ToTokens for Enum {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = Ident::from(&*self.name);
        let vars: Vec<_> = self.variants
            .iter()
            .map(|v| {
                let name = &v.0;
                let tags = &v.1;
                quote! {
                #(#tags),*
                #name
            }
            })
            .collect();
        let tok =
            quote! {
            pub enum #name {
                #(#vars),*
            }
        };
        tokens.append(tok);
    }
}

fn beautify(t: Tokens) -> Result<String> {
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
        let id1: Uri = "#/this/is/some/route/my type".into();
        assert_eq!(id1.to_type_name("root").unwrap(), "ThisIsSomeRouteMyType");
        let id2: Uri = "#/properties/aProp".into();
        assert_eq!(id2.to_type_name("root").unwrap(), "PropertyAprop");
        let id3: Uri = "#".into();
        assert_eq!(id3.to_type_name("root").unwrap(), "Root");
        // TODO make this work
        // let id4: Uri = "#more".into();
        // assert_eq!(id3.to_name("root"), "More");
    }

    #[test]
    fn test_simple_schema() {
        let root = RootSchema::from_file_yaml("test_schemas/simple.yaml").unwrap();
        root.generate("TestSimple").unwrap();
    }

    #[test]
    fn test_meta_schema() {
        let root = RootSchema::from_file_yaml("test_schemas/metaschema-draft4.json").unwrap();
        root.generate("Schema").unwrap();
    }
}
