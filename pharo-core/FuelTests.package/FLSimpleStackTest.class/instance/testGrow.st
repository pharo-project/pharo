tests-adding
testGrow

	| stack |
	stack := FLSimpleStack new: 1.
	self assert: 1 = stack capacity.
	stack push: #a.
	self assert: 1 < stack capacity.