accessing method dictionary
localMethodNamed: selector ifAbsent: aBlock
	"Answer the locally defined method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary or it is not a local one, return the value of aBlock"
	| method |
	method := self compiledMethodAt: selector ifAbsent: aBlock.
	
	self basicLocalSelectors ifNil: [^method].
	
	(self basicLocalSelectors includes: selector) ifTrue: [ ^method ].
	
	^aBlock value