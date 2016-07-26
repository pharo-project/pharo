printing
printOn: aStream 
	aStream
		nextPutAll: self displayClassName; nextPut: $ ;
		nextPutAll: self changeSymbol; nextPut: $ ;
		print: self variable;
		nextPut: $!