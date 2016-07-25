utilities
fastWrite: object toFileNamed: path
	^ path asFileReference
		writeStreamDo: [ :fileStream | 
			ZnBufferedWriteStream
				on: fileStream
				do: [ :stream | 
					STON writer
						on: stream;
						optimizeForLargeStructures;
						nextPut: object ] ]