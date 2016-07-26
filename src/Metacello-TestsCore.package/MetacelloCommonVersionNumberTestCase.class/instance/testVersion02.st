tests
testVersion02

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)