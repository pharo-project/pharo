instance creation
to: aPackage oldName: aSymbol newName: anotherSymbol

	^ (self to: aPackage) 
		oldName: aSymbol;
		newName: anotherSymbol;
		yourself