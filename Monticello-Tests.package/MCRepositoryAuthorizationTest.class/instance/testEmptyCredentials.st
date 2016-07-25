tests
testEmptyCredentials

	| repo |
	"Although you can do this, it's easier to use #location:. See #testNoCredentials"
	repo := MCHttpRepository location: self exampleServerUrl user: '' password: ''.
	self assert: repo user equals: ''.
	self assert: repo password equals: ''.