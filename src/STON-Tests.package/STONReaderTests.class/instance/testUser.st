tests
testUser
	| user |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user