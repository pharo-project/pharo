public access
encoder
	^encoder ifNil:
		[encoder := EncoderForV3PlusClosures new]