matching
copyInContext: aDictionary
	| keywordSelector |
	self isList 
		ifTrue: [ ^ aDictionary at: self ].
	keywordSelector := self isSelectorList 
		ifTrue: [ aDictionary at: self selectorParts first ]
		ifFalse: [ '' join: (self selectorParts collect: [ :each | aDictionary at: each ])].
	^ RBMessageNode
		receiver: (self receiver copyInContext: aDictionary)
		selector: keywordSelector
		arguments: (self copyList: self arguments inContext: aDictionary)