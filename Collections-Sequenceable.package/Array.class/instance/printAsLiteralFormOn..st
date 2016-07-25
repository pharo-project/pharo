self evaluating
printAsLiteralFormOn: aStream
	aStream nextPut: $#.
	self printElementsOn: aStream
