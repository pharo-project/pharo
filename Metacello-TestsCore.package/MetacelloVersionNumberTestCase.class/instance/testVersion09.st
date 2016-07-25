tests
testVersion09

	| v1 v2 |
	v1 := self versionClass fromString: '1.0'.
	v2 := self versionClass fromString: '0.7'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)