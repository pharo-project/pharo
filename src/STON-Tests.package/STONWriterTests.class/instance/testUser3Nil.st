tests
testUser3Nil
	| user |
	user := STONTestUser3 new.
	self 
		assert: (self serialize: user) 
		equals: 'TestUser3{#username:nil,#password:nil,#enabled:true}'