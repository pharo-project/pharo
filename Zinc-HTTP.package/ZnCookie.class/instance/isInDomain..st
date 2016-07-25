testing
isInDomain: urlObject
	| domain |
	domain := (self domain beginsWith: '.')
		ifTrue: [ self domain copyFrom: 2 to: self domain size ]
		ifFalse: [ self domain ].
	^ urlObject host includesSubstring: domain
	