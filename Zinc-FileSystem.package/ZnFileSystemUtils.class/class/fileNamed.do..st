streams
fileNamed: fileName do: block
	^ fileName asFileReference 
		writeStreamDo: block