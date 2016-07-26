instance creation
newWithEncoderClass: class
	^ self basicNew 
		encoderClass: class;
		initialize;
		yourself