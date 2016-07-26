tests - storing
testStoreSettingNodeTwice
	| fileSize fullPath |
	self deny: preferences exists.
	fullPath := preferences fullPath.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferences fullPath equals: fullPath.
	self assert: preferences exists.
	fileSize := preferences size.
	self assert: fileSize > 0.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferences fullPath equals: fullPath.
	self assert: preferences size equals: fileSize.