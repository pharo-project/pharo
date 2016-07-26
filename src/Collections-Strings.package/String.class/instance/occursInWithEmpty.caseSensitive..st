testing
occursInWithEmpty: prefix caseSensitive: aBoolean 
	"Answer whether the receiver begins with the given prefix string. 
	The comparison is case-sensitive." 
	| matchTable |
	prefix isEmpty ifTrue: [ ^ true ].
	self size < prefix size ifTrue: [ ^ false ].
	matchTable := aBoolean 
		ifTrue: [ CaseSensitiveOrder ]
		ifFalse: [ CaseInsensitiveOrder ].
	^ (self 
		findSubstring: prefix
		in: self
		startingAt: 1
		matchTable: matchTable) > 0