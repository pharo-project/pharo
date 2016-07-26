testing
testComparison
	|a b|
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	
	self assert: a = a copy.
	self assert: a <= a copy.
	self assert: a >= a copy.
	self assert: b = b copy.
	self assert: b <= b copy.
	self assert: b >= b copy.
	self assert: a < b.
	self assert: a <= b.
	self assert: b > a.
	self assert: b >= a.
	
	self deny: (a > b) = (b > a).
	self deny: (a >= b) = (b >= a).
	
	