tests
testVersion08

	| v1 v2 |
	v1 := self versionClass fromString: '1.0a6'.
	v2 := self versionClass fromString: '1.0a5'.
	self assert: (v1 > v2)