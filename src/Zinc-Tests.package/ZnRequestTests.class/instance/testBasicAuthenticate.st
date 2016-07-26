testing
testBasicAuthenticate
	| request |
	request := ZnRequest new.
	request setBasicAuthenticationUsername: 'user' password: 'secret'.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjpzZWNyZXQ='