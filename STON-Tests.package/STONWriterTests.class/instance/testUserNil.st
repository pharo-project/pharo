tests
testUserNil
	| user |
	user := STONTestUser new.
	self assert: (self serialize: user) equals: 'TestUser{#enabled:true}'