testing
hasMultipleReturns
	| count |
	count := 0.
	self nodesDo: [:each | each isReturn ifTrue: [count := count + 1]].
	^count > 1