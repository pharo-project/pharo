I am ZnPercentEncoder.
I implement RFC 3986 percent encoding and decoding.

All characters that are not part of a safe set are encoded using a percent (%) followed by a two digit hexadecimal number of a byte value. Non-ASCII characters are first encoded, normally using UTF-8.

My #encode: and #decode: messages work from String to String.

My decoder will accept + as an encoding for a space by default.

See also http://en.wikipedia.org/wiki/Percent-encoding

Part of Zinc HTTP Components
