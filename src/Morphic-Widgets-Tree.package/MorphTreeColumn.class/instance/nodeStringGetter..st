accessing
nodeStringGetter: aSelectorOrAValuable
	rowMorphGetSelector := aSelectorOrAValuable isSymbol 
		ifTrue: [[:node | (node complexContents perform: aSelectorOrAValuable) asMorph]]
		ifFalse: [[:node | (aSelectorOrAValuable value: node complexContents ) asMorph]]