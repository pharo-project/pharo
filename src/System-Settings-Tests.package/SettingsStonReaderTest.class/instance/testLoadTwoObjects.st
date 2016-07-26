tests
testLoadTwoObjects
	reader stream: MockSettings twoStoredSettingsSTONString readStream.
	reader load.
	self assert: reader storedSettings notNil.
	self assert: reader storedSettings size equals: 2.
	self assert: reader storedSettings first realValue equals: false.
	self assert: reader storedSettings second realValue equals: 55.