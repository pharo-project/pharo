conversions
decodeBase64: string
	| bytes |
	bytes := ZnBase64Encoder new decode: string.
	^ ZnNullEncoder new decodeBytes: bytes