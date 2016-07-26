instance creation
remove: aSymbol from: aClass
	^ self new
		changeClass: aClass;
		selector: aSymbol;
		yourself