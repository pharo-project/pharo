utilities
fastReadFromFileNamed: path
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: (ZnBufferedReadStream on: stream);
				optimizeForLargeStructures;
				next ]