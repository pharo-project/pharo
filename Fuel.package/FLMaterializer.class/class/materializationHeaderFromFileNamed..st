materializing-shortcuts
materializationHeaderFromFileNamed: aFilename

	^ FLPlatform current
		fileNamed: aFilename 
		readStreamDo: [ :aFileStream | 
			self newDefault materializeHeaderFrom: aFileStream ]
