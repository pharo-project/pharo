tests
testStoreTwoObjects
	| settingNode1 settingNode2 |
	settingNode1 := MockSettings booleanSettingNode.
	settingNode2 := MockSettings rangeSettingNode.
	writer addSetting: ((StoredSettingsFactory new from: settingNode1) realValue: false; yourself).
	writer addSetting: ((StoredSettingsFactory new from: settingNode2) realValue: 55; yourself).
	writer store.
	
	self assert: writer stream contents equals: MockSettings twoStoredSettingsSTONString.