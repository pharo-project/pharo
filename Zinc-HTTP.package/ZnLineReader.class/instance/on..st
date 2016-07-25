initialize-release
on: readStream
	stream := readStream.
	buffer := String new: 64.
	self 
		limit: ZnConstants maximumLineLength; 
		reset