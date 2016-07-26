method generation
symbolicMethodSelectorAndPragma: aSelector symbolicVersionSymbol: symbolicVersionSymbol on: strm
	strm
		nextPutAll: aSelector asString , ' spec';
		cr;
		tab;
		nextPutAll: '<symbolicVersion: #' , symbolicVersionSymbol asString printString;
		nextPutAll: '>';
		cr