testing
testPush

	| stack |
	stack := FLSimpleStack new.
	stack push: 1.
	self deny: stack isEmpty.