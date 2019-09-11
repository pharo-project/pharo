Class {
	#name : #ZnBase64EncoderTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #benchmarks }
ZnBase64EncoderTest class >> bench10kDecode [
	"self bench10kDecode"
	
	| string alphabet |
	alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'. 
	string := String new: 10240 streamContents: [ :out |
		10240 timesRepeat: [ out nextPut: alphabet atRandom ] ].
	^ [ ZnBase64Encoder new decode: string ] bench
]

{ #category : #benchmarks }
ZnBase64EncoderTest class >> bench10kEncode [
	"self bench10kEncode"
	
	| bytes |
	bytes := ByteArray new: 10240 streamContents: [ :out |
		10240 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	^ [ ZnBase64Encoder new encode: bytes ] bench
]

{ #category : #benchmarks }
ZnBase64EncoderTest class >> bench1kDecode [
	"self bench1kDecode"
	
	| string alphabet |
	alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'. 
	string := String new: 1024 streamContents: [ :out |
		1024 timesRepeat: [ out nextPut: alphabet atRandom ] ].
	^ [ ZnBase64Encoder new decode: string ] bench
]

{ #category : #benchmarks }
ZnBase64EncoderTest class >> bench1kEncode [
	"self bench1kEncode"
	
	| bytes |
	bytes := ByteArray new: 1024 streamContents: [ :out |
		1024 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	^ [ ZnBase64Encoder new encode: bytes ] bench
]

{ #category : #benchmarks }
ZnBase64EncoderTest class >> benchmarks [
	"self benchmarks"
	
	^ (#( bench1kEncode bench1kDecode bench10kEncode bench10kDecode )
		 collect: [ :each | each -> (self perform: each) ]) asDictionary
]

{ #category : #testing }
ZnBase64EncoderTest >> testCustomLineBreaking [
	| encoder input output charCount |
	encoder := ZnBase64Encoder new.
	encoder breakLinesAt: 16.
	input := (0 to: 255) asByteArray.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
	charCount := ((256 // 3) + (256 \\ 3) sign) * 4.
	self assert: output size equals: (charCount + (charCount // 16 * String crlf size))
]

{ #category : #testing }
ZnBase64EncoderTest >> testDecodingErrors [
	| encoder |
	encoder := ZnBase64Encoder new.
	self should: [ encoder decode: 'A' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'AB' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'ABC' ] raise: ZnCharacterEncodingError.
	encoder whitespace: #separator.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.
	encoder whitespace: nil.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.
]

{ #category : #testing }
ZnBase64EncoderTest >> testEmpty [
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: #[])
		equals: ''.
	self
		assert: (encoder decode: '')
		equals: #[]
]

{ #category : #testing }
ZnBase64EncoderTest >> testFullAlphabet [
	| encoder input output |
	encoder := ZnBase64Encoder new.
	input := encoder alphabet.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input.
	encoder breakLines.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input
]

{ #category : #testing }
ZnBase64EncoderTest >> testFullSpectrum [
	| encoder input output |
	encoder := ZnBase64Encoder new.
	input := (0 to: 255) asByteArray , (255 to: 0) asByteArray.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
	encoder breakLines.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.

]

{ #category : #testing }
ZnBase64EncoderTest >> testPadding [
	| encoder |
	encoder := ZnBase64Encoder new.
	self assert: (encoder encode: 'M' asByteArray) equals: 'TQ=='.
	self assert: (encoder decode: 'TQ==') equals: 'M' asByteArray.
	self assert: (encoder encode: 'Ma' asByteArray) equals: 'TWE='.
	self assert: (encoder decode: 'TWE=') equals: 'Ma' asByteArray
]

{ #category : #testing }
ZnBase64EncoderTest >> testQuote [
	| input output encoder break |
	encoder := ZnBase64Encoder new lineEndConvention: #cr; breakLines; yourself.
	input := 'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.'.
	break := String with: Character cr.
	output := 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz' , break 
				, 'IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg' , break 
				, 'dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu' , break 
				, 'dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo' , break 
				, 'ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='.
	self 
		assert: (encoder encode: input asByteArray)
		equals: output.
	self 
		assert: (encoder decode: output) 
		equals: input asByteArray 
]

{ #category : #testing }
ZnBase64EncoderTest >> testSimple [
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: 'Man' asByteArray)
		equals: 'TWFu'.
	self
		assert: (encoder decode: 'TWFu')
		equals: 'Man' asByteArray 
]

{ #category : #testing }
ZnBase64EncoderTest >> testWhitespaceAtEnd [
	| encoder |
	encoder := ZnBase64Encoder new.
	"whitespace is #any non-alphabet character"
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: #separator.
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: nil.
	self should: [ encoder decode: 'TQ==' , String lf ] raise: ZnCharacterEncodingError 
]
