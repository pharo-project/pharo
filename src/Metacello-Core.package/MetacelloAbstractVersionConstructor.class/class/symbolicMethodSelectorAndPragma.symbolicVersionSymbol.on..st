method generation
symbolicMethodSelectorAndPragma: selector symbolicVersionSymbol: symbolicVersionSymbol on: strm
	strm
		nextPutAll: selector asString , ' spec';
		cr;
		tab;
		nextPutAll: '<symbolicVersion: #' , symbolicVersionSymbol asString printString;
		nextPutAll: '>';
		cr