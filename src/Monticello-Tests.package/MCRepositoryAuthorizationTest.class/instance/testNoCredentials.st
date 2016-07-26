tests
testNoCredentials

	| repo |
	repo := MCHttpRepository location: self exampleServerUrl.
	self assert: repo user equals: ''.
	self assert: repo password equals: ''.