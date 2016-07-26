tests
testHash
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		self assert: aClass new hash equals: aClass new hash ]