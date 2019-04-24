# Purescript-Motsunabe

[![Build Status](https://travis-ci.com/justinwoo/purescript-motsunabe.svg?branch=master)](https://travis-ci.com/justinwoo/purescript-motsunabe)

A trivial implementation of Wadler's Prettier Printer.

![](https://i.imgur.com/f8u4eCv.jpg)

## Example

from tests:

```purs
main = do
  assert $ pretty 80 emptyDoc == "\n"
  assert $ prettyWithoutTrailing 80 emptyDoc == ""
  assert $ pretty 80 stringDoc == "hello i am 24 chars long\n"
  assert $ pretty 80 alt == "hello i am 24 chars longhello i am 24 chars long\n"
  assert $ pretty 30 alt == "hello i am 24 chars long\nhello i am 24 chars long\n"
  assert $ pretty 80 nest == "hello i am 24 chars long\n  hello i am 24 chars long\n"
  where
    emptyDoc = DNil
    stringDoc = DText "hello i am 24 chars long"
    alt = DAlt (stringDoc <> stringDoc) (stringDoc <> DLine <> stringDoc)
    nest = stringDoc <> DNest 1 (DLine <> stringDoc)
```
