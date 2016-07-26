convenience
mimeDecode: aStringOrStream to: outStream 
	self new
		mimeStream: (aStringOrStream isStream 
				ifTrue: [ aStringOrStream ]
				ifFalse: [ aStringOrStream readStream ]);
		dataStream: outStream;
		mimeDecode