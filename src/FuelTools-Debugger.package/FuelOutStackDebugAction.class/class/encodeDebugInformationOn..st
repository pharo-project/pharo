private
encodeDebugInformationOn: aSerializer
	| str |
	
	str := String new writeStream. 
	str print: Date today; space; print: Time now.
	aSerializer at: #Timestamp putAdditionalObject: str contents. 
	
	str := String new writeStream. 
	str nextPutAll: 'VM: ';
		nextPutAll: Smalltalk os name asString;
		nextPutAll: ' - ';	
		nextPutAll: Smalltalk os subtype asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk os version asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vm version asString.
	aSerializer at: #VM putAdditionalObject: str contents. 
		
	str := String new writeStream. 
	str nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']'.
	aSerializer at: #Image putAdditionalObject: str contents.