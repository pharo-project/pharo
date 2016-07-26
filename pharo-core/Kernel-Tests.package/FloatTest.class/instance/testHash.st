tests
testHash
	self assert: 2 = 2.0 ==> (2 hash = 2.0 hash).
	self assert: 1 / 2 = 0.5 ==> ((1 / 2) hash = 0.5 hash).
	Float nan hash.
	Float infinity hash