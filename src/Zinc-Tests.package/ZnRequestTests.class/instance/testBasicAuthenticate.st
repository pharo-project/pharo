testing
testBasicAuthenticate
	| request |
	request := ZnRequest new.
	request setBasicAuthenticationUsername: 'user' password: 'secret'.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjpzZWNyZXQ='.
	self assert: request basicAuthentication asArray equals: #( 'user' 'secret' ).
	request setBasicAuthenticationUsername: 'user' password: ''.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjo='.
	self assert: request basicAuthentication asArray equals: #( 'user' '' ).
	request setBasicAuthenticationUsername: 'user' password: ':colons:'.
	self assert: (request headers at: 'Authorization') = 'Basic dXNlcjo6Y29sb25zOg=='.
	self assert: request basicAuthentication asArray equals: #( 'user' ':colons:' ).