use std::{collections::BTreeSet, convert::TryFrom, iter::FromIterator};

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    bracketed,
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse2,
    spanned::Spanned,
    token::Token,
    Path,
};
use syn::{punctuated::Punctuated, Error, Expr, Ident, LitStr, Result, Token};

use crate::WithSpan;

pub enum SpannedConstraint {
    Init(WithSpan<ConstraintInit>),
    Mut(WithSpan<ConstraintMut>),
    Signer(WithSpan<ConstraintSigner>),
    Address(WithSpan<ConstraintAddress>),
    Seeds(WithSpan<ConstraintSeeds>),
    Bump(WithSpan<ConstraintBump>),
    BumpSave(WithSpan<ConstraintBumpSave>),
    BelongsTo(WithSpan<ConstraintBelongsTo>),
    Owner(WithSpan<ConstraintOwner>),
    RentExempt(WithSpan<ConstraintRentExempt>),
    Expr(WithSpan<ConstraintExpr>),
}

impl Spanned for SpannedConstraint {
    fn span(&self) -> Span {
        match self {
            SpannedConstraint::Init(c) => c.span(),
            SpannedConstraint::Mut(c) => c.span(),
            SpannedConstraint::Signer(c) => c.span(),
            SpannedConstraint::Address(c) => c.span(),
            SpannedConstraint::Seeds(c) => c.span(),
            SpannedConstraint::Bump(c) => c.span(),
            SpannedConstraint::BumpSave(c) => c.span(),
            SpannedConstraint::BelongsTo(c) => c.span(),
            SpannedConstraint::Owner(c) => c.span(),
            SpannedConstraint::RentExempt(c) => c.span(),
            SpannedConstraint::Expr(c) => c.span(),
        }
    }
}

pub trait Constraint: Sized {
    const NAME: &'static str;

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>>;
}

impl Parse for SpannedConstraint {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.call(Ident::parse_any)?;
        macro_rules! try_parse {
            ($ident:ident, $input:ident, $constraint:ty, $brach:ident) => {
                if $ident == <$constraint as Constraint>::NAME {
                    return Ok(Self::$brach(<$constraint as Constraint>::continue_parse(
                        input,
                        ident.span(),
                    )?));
                }
            };
        }
        try_parse!(ident, input, ConstraintInit, Init);
        try_parse!(ident, input, ConstraintMut, Mut);
        try_parse!(ident, input, ConstraintSigner, Signer);
        try_parse!(ident, input, ConstraintAddress, Address);
        try_parse!(ident, input, ConstraintSeeds, Seeds);
        try_parse!(ident, input, ConstraintBump, Bump);
        try_parse!(ident, input, ConstraintBumpSave, BumpSave);
        try_parse!(ident, input, ConstraintBelongsTo, BelongsTo);
        try_parse!(ident, input, ConstraintOwner, Owner);
        try_parse!(ident, input, ConstraintRentExempt, RentExempt);
        try_parse!(ident, input, ConstraintExpr, Expr);

        Err(Error::new(ident.span(), "Unknown attribute"))
    }
}

#[derive(Debug)]
pub struct ConstraintInit;

impl Constraint for ConstraintInit {
    const NAME: &'static str = "init";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        Ok(WithSpan::new(Self, start_span))
    }
}

#[derive(Debug)]
pub struct ConstraintMut;

impl Constraint for ConstraintMut {
    const NAME: &'static str = "mut";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        Ok(WithSpan::new(Self, start_span))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstraintSigner;

impl Constraint for ConstraintSigner {
    const NAME: &'static str = "signer";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        Ok(WithSpan::new(Self, start_span))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstraintAddress {
    pub value: LitStr,
}

impl Constraint for ConstraintAddress {
    const NAME: &'static str = "address";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let value: LitStr = input.parse()?;
        let value_span = value.span();
        Ok(WithSpan::new(
            Self { value },
            start_span.join(value_span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug)]
pub struct ConstraintSeeds {
    pub seeds: Punctuated<Expr, Token![,]>,
}

impl Constraint for ConstraintSeeds {
    const NAME: &'static str = "seeds";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let content;
        let bracket = bracketed!(content in input);
        Ok(WithSpan::new(
            Self {
                seeds: content.parse_terminated(Expr::parse)?,
            },
            start_span.join(bracket.span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug)]
pub struct ConstraintBump {
    pub expr: Expr,
}

impl Constraint for ConstraintBump {
    const NAME: &'static str = "bump";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let expr: Expr = input.parse()?;
        let expr_span = expr.span();
        Ok(WithSpan::new(
            Self { expr },
            start_span.join(expr_span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug)]
pub struct ConstraintBumpSave {
    pub expr: Expr,
}

impl Constraint for ConstraintBumpSave {
    const NAME: &'static str = "bump_save";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let expr: Expr = input.parse()?;
        let expr_span = expr.span();
        Ok(WithSpan::new(
            Self { expr },
            start_span.join(expr_span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstraintBelongsTo {
    pub join_target: Ident,
}

impl Constraint for ConstraintBelongsTo {
    const NAME: &'static str = "belongs_to";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let end_span = input.span();
        Ok(WithSpan::new(
            Self {
                join_target: input.parse()?,
            },
            start_span.join(end_span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintOwner {
    Program,
    Value(LitStr),
    Skip,
}

impl Constraint for ConstraintOwner {
    const NAME: &'static str = "owner";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            if input.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                let lit_span = lit.span();
                return Ok(WithSpan::new(
                    Self::Value(lit),
                    start_span.join(lit_span).unwrap_or(start_span),
                ));
            }

            let ident: Ident = input.parse()?;
            let span = start_span.join(ident.span()).unwrap_or(start_span);

            match ident.to_string().as_str() {
                "skip" => Ok(WithSpan::new(Self::Skip, span)),
                "program" => Ok(WithSpan::new(Self::Program, span)),
                _ => Err(Error::new(span, "Expected skip or program")),
            }
        } else {
            Ok(WithSpan::new(Self::Program, start_span))
        }
    }
}

#[derive(Debug)]
pub enum ConstraintRentExempt {
    Enforce,
    Skip,
}

impl Constraint for ConstraintRentExempt {
    const NAME: &'static str = "rent_exempt";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            let ident: Ident = input.parse()?;
            let span = start_span.join(ident.span()).unwrap_or(start_span);

            match ident.to_string().as_str() {
                "skip" => Ok(WithSpan::new(Self::Skip, span)),
                _ => Err(Error::new(span, "Expected skip")),
            }
        } else {
            Ok(WithSpan::new(Self::Enforce, start_span))
        }
    }
}

#[derive(Debug)]
pub struct ConstraintExpr(Expr);

impl Constraint for ConstraintExpr {
    const NAME: &'static str = "expr";

    fn continue_parse(input: ParseStream, start_span: Span) -> Result<WithSpan<Self>> {
        input.parse::<Token![=]>()?;
        let expr: Expr = input.parse()?;
        let expr_span = expr.span();
        Ok(WithSpan::new(
            Self(expr),
            start_span.join(expr_span).unwrap_or(start_span),
        ))
    }
}

impl<'a> ToTokens for ConstraintExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}
