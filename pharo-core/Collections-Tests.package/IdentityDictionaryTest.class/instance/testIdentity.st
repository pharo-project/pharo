tests - identity
testIdentity
	| dict key |
	dict := self classToBeTested new.
	key := 'key'.
	dict at: key put: 2.5.
	 
	
	self assert: (dict includesKey: key).
	self deny: (dict includesKey: key copy).
	
"	dict at: 1 put: 'djdh'.
	dict at: 'sksl' put: 1.0.
	self deny: (dict includesKey: 1.0) .
	self assert: (dict includes: 1)"
