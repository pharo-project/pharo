tests
testVersion04

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)