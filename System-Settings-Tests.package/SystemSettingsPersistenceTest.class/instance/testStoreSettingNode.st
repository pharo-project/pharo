tests - storing
testStoreSettingNode
	self deny: preferences exists.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferences exists.
	self assert: preferences size > 0.
	