tests
testPrintOn
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		String streamContents: [ :aStream | aClass new printOn: aStream ] ]