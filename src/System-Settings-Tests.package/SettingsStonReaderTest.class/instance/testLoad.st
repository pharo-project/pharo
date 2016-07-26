tests
testLoad
	reader stream: MockSettings oneStoredSettingSTONString readStream.
	reader load.
	self assert: reader storedSettings notNil.
	self assert: reader storedSettings size equals: 1.
	self assert: reader storedSettings first realValue equals: true.