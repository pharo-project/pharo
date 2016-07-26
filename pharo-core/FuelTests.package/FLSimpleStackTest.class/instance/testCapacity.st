testing
testCapacity

	| stack |
	stack := FLSimpleStack new: 3.
	self assert: 3 = stack capacity.
	
	stack := FLSimpleStack new: 5.
	self assert: 5 = stack capacity.