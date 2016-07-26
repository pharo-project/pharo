tests
testChanges
	| aCollection |
	aCollection := {RBRefactoryChange new}.
	browser changes: aCollection.
	self assert: browser changes equals: aCollection