column drawing
itemMorphGetter: aSelectorOrAValuable
	rowMorphGetSelector := aSelectorOrAValuable isSymbol 
		ifTrue: [[:node | node complexContents item perform: aSelectorOrAValuable]]
		ifFalse: [[:node | aSelectorOrAValuable value: node complexContents item]]