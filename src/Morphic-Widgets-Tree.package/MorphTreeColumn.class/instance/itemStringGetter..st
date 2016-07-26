column drawing
itemStringGetter: aSelectorOrAValuable
	rowMorphGetSelector := aSelectorOrAValuable isSymbol 
		ifTrue: [[:node | (node complexContents item perform: aSelectorOrAValuable) asMorph]]
		ifFalse: [[:node | (aSelectorOrAValuable value: node complexContents item) asMorph]]