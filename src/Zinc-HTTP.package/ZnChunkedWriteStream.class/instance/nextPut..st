accessing
nextPut: element
	self error: 'Wrap me into a buffered stream, because each #nexPut: will create a new chunk'