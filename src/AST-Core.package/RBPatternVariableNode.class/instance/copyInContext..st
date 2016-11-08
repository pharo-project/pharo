matching
copyInContext: aDictionary
	^ (self isList and: [ self isStatement ])
		ifTrue: [ self copyList: (aDictionary at: self) inContext: aDictionary ]
		ifFalse: [ (aDictionary at: self) copy ]