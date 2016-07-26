printing
printOn: aStream
	aStream
		print: self label;
		<< '->';
		<< (self value printStringLimitedTo: 50)