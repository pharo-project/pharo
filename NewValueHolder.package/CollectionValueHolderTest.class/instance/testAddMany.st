tests
testAddMany
	empty
		whenAddedDo:
			[ :addedValue :ann | 
			fired := true.
			self assert: addedValue equals: #(1 2).
			self assert: (ann isMemberOf: ValueAdded) ].
	empty addAll: #(1 2).
	self assert: fired