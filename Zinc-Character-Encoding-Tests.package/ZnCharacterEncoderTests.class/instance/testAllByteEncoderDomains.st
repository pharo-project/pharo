testing
testAllByteEncoderDomains
	| encoder characterDomain byteDomain encoded |
	ZnByteEncoder knownEncodingIdentifiers do: [ :identifier |
		encoder := ZnCharacterEncoder newForEncoding: identifier.
		self assert: encoder identifier equals: identifier.
		self assert: encoder isStrict.
		self assert: encoder isLenient not.
		self assert: (encoder encodeString: 'hello') equals: #[104 101 108 108 111] .
		self assert: (encoder decodeBytes: #[104 101 108 108 111] ) equals: 'hello'.
		characterDomain := encoder characterDomain.
		byteDomain := encoder byteDomain.
		characterDomain do: [ :each |
			self assert: (encoder encodedByteCountFor: each) equals: 1.
			encoded := ByteArray streamContents: [ :out | encoder nextPut: each toStream: out ].
			self assert: encoded size equals: 1.
			self assert: (byteDomain includes: encoded first) ].
		byteDomain do: [ :each |
			self assert: (characterDomain includes: (encoder nextFromStream: (ByteArray with: each) readStream)) ] ]