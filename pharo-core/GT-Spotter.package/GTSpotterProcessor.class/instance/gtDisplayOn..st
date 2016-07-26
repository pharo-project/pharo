printing
gtDisplayOn: stream
	stream 
		nextPutAll: 'Processor: ';
		nextPutAll: self title asString