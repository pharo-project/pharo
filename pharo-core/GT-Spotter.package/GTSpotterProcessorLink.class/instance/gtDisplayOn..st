printing
gtDisplayOn: stream
	stream 
		nextPutAll: 'Processor link: ';
		nextPutAll: self processor title asString