tests
testAddOne
	empty
		whenAddedDo:
			[ :addedValue :ann | 
			fired := true.
			self assert: addedValue equals: 1.
			self assert: (ann isMemberOf: ValueAdded) ].
	empty add: 1.
	self assert: fired