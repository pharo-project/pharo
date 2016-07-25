public
streamError
	reference exists
		ifFalse: [FileDoesNotExist signalWith: reference].
	self error: 'Unable to open file ' , reference printString