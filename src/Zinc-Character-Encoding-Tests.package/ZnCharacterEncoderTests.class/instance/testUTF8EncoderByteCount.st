testing
testUTF8EncoderByteCount	
	| encoder |
	encoder := ZnUTF8Encoder new.
	self assert: (encoder encodedByteCountFor: $$) = 1.
	self assert: (encoder encodedByteCountFor: (16r00A2 asCharacter)) = 2.
	self assert: (encoder encodedByteCountFor: (16r20AC asCharacter)) = 3.
	self assert: (encoder encodedByteCountFor: (16r024B62 asCharacter)) = 4