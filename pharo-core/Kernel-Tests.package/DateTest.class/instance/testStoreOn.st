tests
testStoreOn
	self assert: (String streamContents: [ :str | january23rd2004 storeOn: str ]) equals: '''23 January 2004'' asDate'