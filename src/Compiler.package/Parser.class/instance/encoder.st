public access
encoder
	^encoder ifNil:
		[encoder := LegacyEncoderForV3PlusClosures new]