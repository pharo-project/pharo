printing
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut: $(;
		cr; print: self a11; tab; print: self a12; tab; print: self a13;
		cr; print: self a21; tab; print: self a22; tab; print: self a23;
		cr; nextPut:$).