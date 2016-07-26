printing
printOn: aStream
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeInstVarName: ';
		print: self oldName;
		nextPut: $!; cr.
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addInstVarName: ';
		print: self newName;
		nextPut: $!