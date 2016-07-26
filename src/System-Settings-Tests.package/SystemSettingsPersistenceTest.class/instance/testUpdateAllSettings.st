tests - loading
testUpdateAllSettings
	self testRealValues.
	self testRealValueModification.
	systemSettings storeSettingNodes.
	MockSettings cleanUp.
	self testRealValues.
	systemSettings updateSettingNodes.
	self assert: self booleanSettingNode realValue equals: false.
	self assert: self rangeSettingNode realValue equals: 55