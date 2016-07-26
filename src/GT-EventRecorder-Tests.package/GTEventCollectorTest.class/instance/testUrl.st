tests
testUrl
	self assert: collector url notNil.
	self assert: collector url equals: GTEventRecorderSettings url.