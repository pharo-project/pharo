tests-Getting Started
testFileStream

	| materializedString |
	FLPlatform current
		fileNamed: 'demo.fuel'
		writeStreamDo: [ :aStream |
			FLSerializer newDefault 
				serialize: 'stringToSerialize' 
	 			on: aStream ].

	FLPlatform current
		fileNamed: 'demo.fuel'
		readStreamDo: [ :aStream |
			materializedString := (FLMaterializer newDefault 
				materializeFrom: aStream ) root ]