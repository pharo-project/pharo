instance creation
named: aSymbol 
	self checkValidName: aSymbol.
	^ self new
		name: aSymbol;
		yourself