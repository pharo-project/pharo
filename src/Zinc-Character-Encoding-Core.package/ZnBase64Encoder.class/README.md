I am ZnBase64Encoder.

Base64 encoding is a technique to encode binary data as a string of characters that can be safely transported over various protocols. Basically, every 3 bytes are encoded using 4 characters from an alphabet of 64. Each encoded character represents 6 bits.

The most commonly used alphabet is 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'. One or two equal signs (= or ==) are used for padding.

  ZnBase64Encoder new encode: #[0 1 2 3 4 5].
  ZnBase64Encoder new encode: #[10 20]
  ZnBase64Encoder new decode: 'BQQDAgEA'.
  ZnBase64Encoder new decode: 'FAo='.

The encoded data can optionally be broken into lines. Characters not part of the alphabet are considered as white space and are ignored when inbetween groups of 4 characters.

My #encode: method works from ByteArray to String, while my #decode: method works from String to ByteArray.

Note that to encode a String as Base64, you first have to encode the characters as bytes using a character encoder.

See also http://en.wikipedia.org/wiki/Base64

Part of Zinc HTTP Components.