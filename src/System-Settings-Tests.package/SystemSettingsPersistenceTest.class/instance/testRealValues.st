tests
testRealValues
	self assert: (systemSettings nodeNamed: #booleanSetting) realValue equals: true.
	self assert: (systemSettings nodeNamed: #rangeSetting) realValue equals: 10.
	