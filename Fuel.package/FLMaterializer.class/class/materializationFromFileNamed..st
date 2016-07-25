materializing-shortcuts
materializationFromFileNamed: aFilename
	^ FLPlatform current
		fileNamed: aFilename 
		readStreamDo: [ :aFileStream | 
			self newDefault materializeFrom: aFileStream ]
