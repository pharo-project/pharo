tests - growable
testAddEmptyGrows
	"self debug: #testAddEmptyGrows"
	
	| oldSize |
	oldSize := self empty size.
	self empty add: self element.
	self assert: (self empty size) = (oldSize + 1).