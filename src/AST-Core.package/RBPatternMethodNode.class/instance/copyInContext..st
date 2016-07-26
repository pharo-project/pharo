matching
copyInContext: aDictionary 
	| keywordSelector |
	keywordSelector := self isSelectorList 
		ifTrue: [ aDictionary at: self selectorParts first ]
		ifFalse: [ ('' join:(self selectorParts collect: [ :each | aDictionary at: each ])) asSymbol  ].
	^ RBMethodNode new
		selector: keywordSelector;
		arguments: (self copyList: self arguments inContext: aDictionary);
		pragmas: (self pragmas isEmpty
			ifTrue: [ aDictionary at: '-pragmas-' ifAbsent: [ #() ] ]
			ifFalse: [ self copyList: self pragmas inContext: aDictionary ]);
		body: (self body copyInContext: aDictionary);
		source: (aDictionary at: '-source-');
		yourself