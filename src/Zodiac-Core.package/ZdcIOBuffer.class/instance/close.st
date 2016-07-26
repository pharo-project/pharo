initialize-release
close
	buffer ifNotNil: [  
		ZdcByteArrayManager current recycle: buffer.
		buffer := nil.
		self reset ]