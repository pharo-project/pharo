initializing
initializeClassNamed: aSymbol toClass: aClass variables: anArray 
	
	self initialize.
	sourceClassName := aSymbol.
	targetClass := aClass.
	variables := anArray.