tests
testStoredValueForSetting
	| value |
	self testStoreSettingNode.
	value := systemSettings storedValueForSettingNode: MockSettings booleanSettingNode.
	self assert: value.