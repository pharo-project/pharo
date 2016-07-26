conversions
encodeBase64: string
	| bytes |
	bytes := ZnNullEncoder new encodeString: string.
	^ ZnBase64Encoder new encode: bytes