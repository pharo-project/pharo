tests
testSettingNodeIdentifierForChildNode
	self assert: MockSettings cacheDirectorySettingNode settingNodeIdentifier equals: '#monticello#cacheDirectory'.
	