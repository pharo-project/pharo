testing
testDefault
	| string bytes |
	self assert: ZnCharacterEncoder default equals: ZnUTF8Encoder new.
	string := 'Der Weg zur Hölle ist mit guten Vorsätzen gepflastert.'.
	bytes := ZnUTF8Encoder new encodeString: string.
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [ 
			self 
				assert: (ZnCharacterEncoder default decodeBytes: bytes) 
				equals: string ].
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [ 
			self 
				assert: ((ZnCharacterEncoder newForEncoding: 'unknown') decodeBytes: bytes) 
				equals: string ]