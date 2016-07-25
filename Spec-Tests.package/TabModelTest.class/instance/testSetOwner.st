tests
testSetOwner
	"issue 16553"

	| tab button |
	button := ButtonModel new.
	tab := TabModel new.

	self assert: button owner equals: nil.
	tab model: button.
	self assert: button owner equals: tab