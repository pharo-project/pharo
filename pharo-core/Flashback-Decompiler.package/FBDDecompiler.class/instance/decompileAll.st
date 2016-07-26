public api
decompileAll
	Smalltalk globals allClassesAndTraits
		do: [ :classOrTrait | self decompileThenRecompileClass: classOrTrait ]
		displayingProgress: 'Decompiling all classes and traits'