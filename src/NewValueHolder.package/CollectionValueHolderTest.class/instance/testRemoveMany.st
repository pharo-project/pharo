tests
testRemoveMany
	withItems
		whenRemovedDo:
			[ :removedValue :ann | 
			fired := true.
			self assert: withItems asArray equals: #(2).
			self assert: removedValue equals: #(1 3).
			self assert: (ann isMemberOf: ValueRemoved) ].
	withItems removeAll: #(1 3).
	self assert: fired