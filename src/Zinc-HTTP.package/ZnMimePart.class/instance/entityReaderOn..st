private
entityReaderOn: stream
	^ ZnEntityReader new
		headers: self headers;
		stream: stream;
		allowReadingUpToEnd;
		yourself