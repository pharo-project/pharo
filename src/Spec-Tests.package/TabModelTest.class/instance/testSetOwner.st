tests
testSetOwner
	"issue 16553"

	| button |
	button := ButtonModel new.
	self assert: button owner equals: nil.
	testedInstance model: button.
	self assert: button owner equals: testedInstance