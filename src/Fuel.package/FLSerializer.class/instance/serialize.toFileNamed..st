serializing
serialize: anObject toFileNamed: aFilename
	"Serialize the graph starting at the root object received and answers the FLSerialization object"
	FLPlatform current
		fileNamed: aFilename 
		writeStreamDo: [ :aFileStream |
			self serialize: anObject on: aFileStream ]
	