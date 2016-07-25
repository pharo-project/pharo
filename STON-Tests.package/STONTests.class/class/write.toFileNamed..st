utilities
write: object toFileNamed: path
	^ path asFileReference
		writeStreamDo: [ :stream | 
			STON writer
				on: stream;
				nextPut: object ]