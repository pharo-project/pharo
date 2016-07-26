printing
printOn: aStream 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeClassVarName: ';
		print: self oldName;
		nextPut: $!; cr.
	aStream
		nextPutAll: self displayClassName; 
		nextPutAll: ' addClassVarName: '; 
		print: self newName;
		nextPut: $!