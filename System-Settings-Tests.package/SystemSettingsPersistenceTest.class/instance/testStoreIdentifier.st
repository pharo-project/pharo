tests - storing
testStoreIdentifier
	self deny: preferences exists.
	systemSettings storeIdentifier: MockSettings booleanSettingNode settingNodeIdentifier.
	self assert: preferences exists.
	self assert: preferences size > 0.
	