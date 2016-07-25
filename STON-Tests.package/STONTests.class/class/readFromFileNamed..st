utilities
readFromFileNamed: path
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: stream;
				next ]