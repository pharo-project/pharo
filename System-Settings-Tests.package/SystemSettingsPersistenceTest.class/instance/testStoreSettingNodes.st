tests - storing
testStoreSettingNodes
	self deny: preferences exists.
	self testRealValueModification.
	systemSettings storeSettingNodes: { 
		MockSettings booleanSettingNode.
		MockSettings rangeSettingNode }.
	self assert: preferences exists.
	self assert: preferences size > 0.
	self assert: preferences contents equals: MockSettings twoStoredSettingsSTONString
