tests
testIdentity
	| dict key |
	dict := self classToBeTested new.
	key := 'key'.
	dict at: key put: 2.5.
	 
	
	self assert: (dict includesKey: key).
	self deny: (dict includesKey: key copy).