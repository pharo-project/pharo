tests
testUniqueIdentifiers
	| names |
	names := self systemSettingNodeList collect: #settingNodeIdentifier.
	self assert: names size equals: names asSet size.