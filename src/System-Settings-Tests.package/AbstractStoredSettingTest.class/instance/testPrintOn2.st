tests
testPrintOn2
	AbstractStoredSetting allSubclassesDo: [ :aClass | | aString |
		aString := String streamContents: [ :aStream | | anInstance |
			anInstance := aClass new.
			anInstance settingNodeIdentifier: #'print-on-identifier'.
			anInstance printOn: aStream ].
		self assert: (aString includesSubstring: #'print-on-identifier') ]