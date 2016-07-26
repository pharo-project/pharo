tests
testChangeRemoveOne
	withItems
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(2 3).
			self assert: changed equals: 1 ].
	withItems remove: 1.
	self assert: fired