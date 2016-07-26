printing
printOn: aStream indent: indent

	indent timesRepeat: [ aStream tab ].
	aStream
		nextPutAll: self title;
		nextPutAll: ' : ';
		nextPutAll: self label.