Variable length integer encoding
================================

[![Hackage](https://img.shields.io/hackage/v/attoparsec-varword.svg)](
https://gitlab.com/concert/hs-varword#readme)
[![pipeline status](https://gitlab.com/concert/hs-varword/badges/master/pipeline.svg)](
https://gitlab.com/concert/hs-varword/commits/master)

- [![attoparsec coverage](
   https://gitlab.com/concert/hs-varword/badges/master/coverage.svg?job=lts-11.7_attoparsec-varword)](
    https://concert.gitlab.io/hs-varword/attoparsec-varword/index.html) attoparsec
- [![bytestring-builder coverage](
   https://gitlab.com/concert/hs-varword/badges/master/coverage.svg?job=lts-11.7_bytestring-builder-varword)](
    https://concert.gitlab.io/hs-varword/bytestring-builder-varword/index.html) bytestring-builder

This repository consists of two Haskell libraries:
 * [`attoparsec-varword`](
   https://gitlab.com/concert/hs-varword/blob/master/attoparsec-varword/README.md)
   for decoding integers within the
   [`attoparsec`](https://github.com/bos/attoparsec) framework
   [[hackage](
   https://hackage.haskell.org/package/attoparsec-varword)]
 * [`bytestring-builder-varword`](
   https://gitlab.com/concert/hs-varword/blob/master/bytestring-builder-varword/README.md)
   for encoding integers via `Builder`s compatible with
   [`blaze-builder`](https://github.com/lpsmith/blaze-builder)
   [[hackage](
   https://hackage.haskell.org/package/bytestring-builder-varword)]

Motivation
----------

Encoding values of multi-byte types (such as `Word32`) can be quite wasteful.
Often there are many leading zeros which inflate the size of the encoded data.
The encodings in this library drop leading empty bytes from the encoding, which
produces more compact data.

Broadly speaking, the value to be encoding is divided into 7-bit chunks.
The most significant bit of each byte is then used when decoding as a flag to
signal whether more data is still to be read for the value being parsed.

This library presents big-endian (most significant byte first) and little-endian
(least significant bytes first) variants of the encoding scheme. Further, we
include a dense version, [used in parts of `git`](
https://medium.com/@concertdaw/sneaky-git-number-encoding-ddcc5db5329f),
that packs the encoded data a little tighter still.
