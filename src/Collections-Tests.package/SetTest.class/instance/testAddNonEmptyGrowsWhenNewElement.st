tests - growable
testAddNonEmptyGrowsWhenNewElement
	"self debug: #testAddNonEmptyGrowsWhenNewElement"
	| oldSize |
	oldSize := self nonEmpty size.
	self deny: (self nonEmpty includes: self elementNotInForOccurrences).
	self nonEmpty add: self elementNotInForOccurrences.
	self assert: self nonEmpty size > oldSize