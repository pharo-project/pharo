tests
testRealValueModification
	self booleanSettingNode realValue: false.
	self rangeSettingNode realValue: 55.
	self assert: self booleanSettingNode realValue equals: false.
	self assert: self rangeSettingNode realValue equals: 55.
