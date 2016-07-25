tests
testRemoveOne
	withItems
		whenRemovedDo:
			[ :removedValue :ann | 
			fired := true.
			self assert: withItems asArray equals: #(2 3).
			self assert: removedValue equals: 1.
			self assert: (ann isMemberOf: ValueRemoved) ].
	withItems remove: 1.
	self assert: fired