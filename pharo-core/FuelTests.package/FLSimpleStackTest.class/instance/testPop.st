testing
testPop

	| stack |
	stack := FLSimpleStack new.
	stack push: 1.
	stack pop.
	self assert: stack isEmpty.