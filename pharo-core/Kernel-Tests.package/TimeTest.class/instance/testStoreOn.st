tests
testStoreOn
	self assert: (String streamContents: [ :str | aTime storeOn: str ]) equals: '''12:34:56 pm'' asTime'