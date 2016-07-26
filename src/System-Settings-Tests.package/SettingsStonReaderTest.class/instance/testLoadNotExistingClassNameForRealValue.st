tests
testLoadNotExistingClassNameForRealValue
	reader stream: MockSettings notExistingClassStoredSettingSTONString readStream.
	reader load.
	self assert: reader storedSettings notNil.
	self assert: reader storedSettings size equals: 1.
	self should: [  reader storedSettings first realValue ] raise: NotFound.
	reader storedSettings first updateSettingNode: nil.