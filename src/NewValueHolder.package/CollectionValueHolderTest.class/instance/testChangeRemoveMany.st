tests
testChangeRemoveMany
	withItems
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(2).
			self assert: changed equals: #(1 3) ].
	withItems removeAll: #(1 3).
	self assert: fired