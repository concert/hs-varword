Variable length integer encoding
================================

[![Build Status](https://travis-ci.org/concert/hs-varword.svg?branch=master)](
https://travis-ci.org/concert/hs-varword)

This repository consists of two Haskell libraries:
 * [`attoparsec-varword`](
   https://github.com/concert/hs-varword/tree/master/attoparsec-varword#readme)
   for decoding integers within the
   [`attoparsec`](https://github.com/bos/attoparsec) framework and
 * [`bytestring-builder-varword`](
   https://github.com/concert/hs-varword/tree/master/bytestring-builder-varword#readme)
   for encoding integers via `Builder`s compatible with
   [`blaze-builder`](https://github.com/lpsmith/blaze-builder)

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
