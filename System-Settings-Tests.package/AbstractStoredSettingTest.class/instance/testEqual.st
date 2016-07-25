tests
testEqual
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		self assert: aClass new equals: aClass new ]