tests
testStoredValueForSettingNoStoredValue
	| value |
	value := systemSettings storedValueForSettingNode: MockSettings booleanSettingNode.
	self assert: value equals: nil.