tests - subcollections access
testFirstNElements
	"self debug: #testFirstNElements"

	| result |
	result := self moreThan3Elements first: self moreThan3Elements size - 1.
	1 to: result size do: [ :i | self assert: (result at: i) = (self moreThan3Elements at: i) ].
	self assert: result size = (self moreThan3Elements size - 1).
	self should: [ self moreThan3Elements first: self moreThan3Elements size + 1 ] raise: SubscriptOutOfBounds