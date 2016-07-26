testing
testUTF32EncoderWide
	| encoder |
	encoder := ZnUTF32Encoder new.
	{ 
		'abc'. 
		'élève en Français'. 
		'Pra-ská' copy at: 4 put: (Character value: 382); yourself. 
		(Character codePoint: 16r1d11e) asString. "MUSICAL SYMBOL G CLEF"
		'' } do: [ :each | 
			| bytes |
			bytes := self encodeString: each with: encoder. 
			self assert: (encoder decodeBytesIntoWideString: bytes) equals: each ]