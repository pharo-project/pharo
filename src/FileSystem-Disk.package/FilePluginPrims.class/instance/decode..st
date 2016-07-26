encoding primitives
decode: aString

	^ (ZnCharacterEncoder newForEncoding: 'utf8') decodeBytes: aString asByteArray