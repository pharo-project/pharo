tests
testStore
	| settingNode |
	settingNode := MockSettings booleanSettingNode.
	writer addSetting: (StoredSettingsFactory new from: settingNode).
	writer store.
	
	self assert: writer stream contents equals: MockSettings oneStoredSettingSTONString