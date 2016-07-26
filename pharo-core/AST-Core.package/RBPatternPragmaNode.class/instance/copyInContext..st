matching
copyInContext: aDictionary 
	| keywordSelector |
	keywordSelector := self isSelectorList 
		ifTrue: [ aDictionary at: self selectorParts first ]
		ifFalse: [ ('' join: (self selectorParts collect: [ :each | aDictionary at: each ])) asSymbol ].
	^ RBPragmaNode
		selector: keywordSelector
		arguments: (self copyList: self arguments inContext: aDictionary)