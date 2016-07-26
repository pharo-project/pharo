utility
createClassNamed: aSymbol superclass: aClass uses: aTraitComposition
	| class |
	class := aClass
		subclass: aSymbol
		uses: aTraitComposition
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''		
		category: self categoryName.
	self createdClassesAndTraits add: class.
	^class