tests
testKeys
	| env keys |
	
	env := self instance.
	keys := env keys.
	
	self deny: keys isEmpty.
	self assert: keys anyOne isString.
	self assert: (env includesKey: keys anyOne)