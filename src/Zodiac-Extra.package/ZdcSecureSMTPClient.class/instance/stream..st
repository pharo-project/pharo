accessing
stream: aStream
	super stream: aStream.
	lineReader := ZnLineReader on: self stream