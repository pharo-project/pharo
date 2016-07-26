public access
encoderClass: anEncoderClass
	encoder ifNotNil: [
		self error: 'encoder already set'].
	encoder := anEncoderClass new