testing
testInstalledMethodsWithIncorrectTrailerKind
	| incorrectMethods |
	
	incorrectMethods := SystemNavigation new installedMethodsWithIncorrectTrailerKind.
	
	self 
		assert: incorrectMethods isEmpty
		description: [ String streamContents: [ :s|
			s 
				nextPutAll: 'Found methods with incorrect trailer kind: ';
				print: incorrectMethods ]].
	