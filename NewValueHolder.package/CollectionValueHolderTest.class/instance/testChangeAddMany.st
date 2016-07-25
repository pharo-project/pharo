tests
testChangeAddMany
	empty
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(1 2 3).
			self assert: changed equals: #(1 2 3) ].
	empty addAll: #(1 2 3).
	self assert: fired