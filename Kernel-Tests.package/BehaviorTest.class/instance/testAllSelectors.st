tests
testAllSelectors
	self assert: ProtoObject allSelectors asSet = ProtoObject selectors asSet.
	self assert: Object allSelectors asSet = (Object selectors asSet union: ProtoObject selectors).
	self assert: (Object allSelectorsBelow: ProtoObject) asSet = (Object selectors) asSet.