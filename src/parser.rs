use crate::{
    Input, ParseResult, and, delimited, fold_many0, fold_many1, many, many1, map, or, or_default,
    peek, separated, separated1, value,
};

/// A Parser is a function that takes an input slice and returns a ParseResult.
pub trait Parser<I, Out> {
    fn parse(&self, input: I) -> ParseResult<I, Out>;
}

impl<I, F, Out> Parser<I, Out> for F
where
    F: Fn(I) -> ParseResult<I, Out>,
{
    fn parse(&self, input: I) -> ParseResult<I, Out> {
        self(input)
    }
}

/// Extension methods for composing parsers with method chaining.
pub trait ParserExt<I, Out>: Parser<I, Out> + Sized {
    fn map<F, NewOut>(self, f: F) -> impl Fn(I) -> ParseResult<I, NewOut>
    where
        I: Input,
        F: Fn(Out) -> NewOut,
    {
        map(self, f)
    }

    fn value<F, Value>(self, val_fn: F) -> impl Fn(I) -> ParseResult<I, Value>
    where
        I: Input,
        F: Fn() -> Value,
    {
        value(self, val_fn)
    }

    fn or<P2>(self, p2: P2) -> impl Fn(I) -> ParseResult<I, Out>
    where
        I: Input,
        P2: Parser<I, Out>,
    {
        or(self, p2)
    }

    fn or_default<D>(self, default: D) -> impl Fn(I) -> ParseResult<I, Out>
    where
        I: Input,
        D: Fn() -> Out,
    {
        or_default(self, default)
    }

    fn and<P2, O2>(self, p2: P2) -> impl Fn(I) -> ParseResult<I, (Out, O2)>
    where
        I: Input,
        P2: Parser<I, O2>,
    {
        and(self, p2)
    }

    fn many(self) -> impl Fn(I) -> ParseResult<I, Vec<Out>>
    where
        I: Input,
    {
        many(self)
    }

    fn many1(self) -> impl Fn(I) -> ParseResult<I, Vec<Out>>
    where
        I: Input,
    {
        many1(self)
    }

    fn separated<S, SepOut>(self, separator: S) -> impl Fn(I) -> ParseResult<I, Vec<Out>>
    where
        I: Input,
        S: Parser<I, SepOut>,
    {
        separated(self, separator)
    }

    fn separated1<S, SepOut>(self, separator: S) -> impl Fn(I) -> ParseResult<I, Vec<Out>>
    where
        I: Input,
        S: Parser<I, SepOut>,
    {
        separated1(self, separator)
    }

    fn delimited_by<P1, P3, O1, O3>(
        self,
        first_delim: P1,
        second_delim: P3,
    ) -> impl Fn(I) -> ParseResult<I, Out>
    where
        I: Input,
        P1: Parser<I, O1>,
        P3: Parser<I, O3>,
    {
        delimited(first_delim, self, second_delim)
    }

    fn peek(self) -> impl Fn(I) -> ParseResult<I, Out>
    where
        I: Input,
    {
        peek(self)
    }

    fn fold_many0<Init, F, Acc>(self, init: Init, f: F) -> impl Fn(I) -> ParseResult<I, Acc>
    where
        I: Input,
        Init: Fn() -> Acc,
        F: Fn(Acc, Out) -> Acc,
    {
        fold_many0(self, init, f)
    }

    fn fold_many1<Init, F, Acc>(self, init: Init, f: F) -> impl Fn(I) -> ParseResult<I, Acc>
    where
        I: Input,
        Init: Fn() -> Acc,
        F: Fn(Acc, Out) -> Acc,
    {
        fold_many1(self, init, f)
    }
}

impl<I, P, Out> ParserExt<I, Out> for P where P: Parser<I, Out> + Sized {}
