use quote::{Tokens, ToTokens, Ident};
use inflector::Inflector;

use errors::*;
use make_valid_identifier;
const HEADER: &str = "use ::serde_json::Value as JsonValue;";

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Variant {
    name: Ident,
    tags: Vec<Tokens>,
    type_: Option<TypeName>,
}

fn strings_to_tokens(strings: Vec<String>) -> Vec<Tokens> {
    strings
        .into_iter()
        .map(|s| {
            let s = Ident::from(s);
            quote! {#s}
        })
        .collect::<Vec<_>>()
}

impl Variant {
    pub(crate) fn new(name: String, tags: Vec<String>, type_: Option<TypeName>) -> Result<Variant> {
        let mut tags = strings_to_tokens(tags);
        let pascal = make_valid_identifier(&name.to_pascal_case())?.into_owned();
        if type_.is_none() && name != pascal {
            tags.push(quote! {
                #[serde(rename = #name)]
            })
        }
        Ok(Variant {
            name: Ident::from(pascal),
            tags,
            type_,
        })
    }
}

impl ToTokens for Variant {
    fn to_tokens(&self, tokens: &mut Tokens) {
        let name = &self.name;
        let tags = &self.tags;
        let tok = if let Some(ref type_) = self.type_ {
            quote! {
                #(#tags),*
                #name(#type_)
            }
        } else {
            quote! {
                #(#tags),*
                #name
            }
        };
        tokens.append(tok);
    }
}

// TODO could remove this and just use Box<ToTokens>
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Renderable {
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

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub(crate) struct TypeName {
    base: String,
    boxed: bool,
    vec: bool,
    option: bool,
}

impl TypeName {
    pub fn new(base: String, required: bool) -> TypeName {
        TypeName {
            base,
            boxed: false,
            vec: false,
            option: !required,
        }
    }

    pub fn array(mut self, arr: bool) -> TypeName {
        self.vec = arr;
        self
    }

    pub fn boxed(mut self, base: &str) -> TypeName {
        self.boxed = self.base == base;
        self
    }
}

impl TypeName {
    pub(crate) fn apply_modifiers(&self) -> String {
        let mut base = self.base.clone();
        if self.vec {
            base = format!("Vec<{}>", base);
        } else if self.boxed {
            base = format!("Box<{}>", base);
        }
        if self.option {
            base = format!("Option<{}>", base);
        }
        base
    }
}

impl ToTokens for TypeName {
    fn to_tokens(&self, tokens: &mut Tokens) {
        tokens.append(Ident::new(self.apply_modifiers()))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    name: Ident,
    tags: Vec<Tokens>,
    pub(crate) fields: Vec<Field>,
}

impl Struct {
    pub(crate) fn new(name: String, tags: Vec<String>, fields: Vec<Field>) -> Struct {
        let name = Ident::from(name.to_pascal_case());
        let mut tags = strings_to_tokens(tags);
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

// TODO get rid of horrid private fields
#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Field {
    pub(crate) name: Ident,
    typename: TypeName,
    tags: Vec<Tokens>,
}

impl Field {
    pub(crate) fn new(name: String, typename: TypeName, tags: Vec<String>) -> Result<Field> {
        let mut tags = strings_to_tokens(tags);
        let snake = make_valid_identifier(&name.to_snake_case())?.into_owned();
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
pub(crate) struct Enum {
    name: Ident,
    tags: Vec<Tokens>,
    variants: Vec<Variant>,
}

impl Enum {
    pub(crate) fn new(name: String, tags: Vec<String>, variants: Vec<Variant>) -> Result<Enum> {
        let mut tags = strings_to_tokens(tags);
        tags.push(quote! {
            #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        });
        Ok(Enum {
            name: Ident::from(name),
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
    pub(crate) fn new(name: String, typename: TypeName, tags: Vec<String>) -> Alias {
        let tags = strings_to_tokens(tags);
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
            pub type #name = #typename;
        };
        tokens.append(tok);
    }
}
