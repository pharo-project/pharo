tests - growable
testAddNonEmptyGrowsWhenNewElement
	"self debug: #testAddNonEmptyGrowsWhenNewElement"
	
	| oldSize |
	oldSize := self nonEmpty size.
	
	self nonEmpty add: self elementNotIn.
	self assert: (self nonEmpty size) > oldSize.