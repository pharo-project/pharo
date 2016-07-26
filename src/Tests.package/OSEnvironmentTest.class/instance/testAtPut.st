tests
testAtPut
	| key value env |
	
	env := self instance.
	value := 1000 atRandom asString.
	key := 'OSEnvironmentTest >> #testAtPut >> ', value.
	
	env at: key put: value.
	self assert: (env at: key) equals: value.
	
	env removeKey: key.
	self deny: (env includesKey: key)