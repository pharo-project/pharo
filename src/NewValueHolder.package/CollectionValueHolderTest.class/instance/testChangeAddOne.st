tests
testChangeAddOne
	empty
		whenChangedDo:
			[ :col :changed | 
			fired := true.
			self assert: col asArray equals: #(1).
			self assert: changed equals: 1 ].
	empty add: 1.
	self assert: fired