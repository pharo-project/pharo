tests
testNoNils
	| d |
	d := WeakIdentityKeyDictionary new
	at: 'hello' copy put: 'world';
		yourself.
	Smalltalk garbageCollect.
	self deny: (d keys includes: nil)