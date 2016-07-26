private
writeToFileSystem
	self fileReference writeStreamDo: [ :stream | 
		stream 
			truncate; 
			nextPutAll: self page contentString ]