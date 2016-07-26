tests - subcollections access
testLastNElements
	"self debug: #testLastNElements"
	| result |
	result := self moreThan3Elements last: self moreThan3Elements size - 1.
	1 
		to: result size
		do: [ :i | self assert: (result at: i) = (self moreThan3Elements at: i + 1) ].
	self assert: result size = (self moreThan3Elements size - 1).
	self 
		should: [ self moreThan3Elements last: self moreThan3Elements size + 1 ]
		raise: Error