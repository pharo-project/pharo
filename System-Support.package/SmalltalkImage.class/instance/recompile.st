compiler
recompile
	Smalltalk globals allClassesAndTraits 
		do: [:classOrTrait | classOrTrait recompile] 
		displayingProgress: 'Recompiling all classes and traits'. 