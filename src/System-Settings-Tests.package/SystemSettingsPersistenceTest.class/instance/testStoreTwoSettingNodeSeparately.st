tests - storing
testStoreTwoSettingNodeSeparately
	| fileSize |
	self deny: preferences exists.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferences exists.
	fileSize := preferences size.
	self assert: fileSize > 0.
	systemSettings storeSettingNode: MockSettings rangeSettingNode.
	self assert: preferences size > fileSize.