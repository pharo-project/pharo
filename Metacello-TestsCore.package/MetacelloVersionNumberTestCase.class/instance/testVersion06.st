tests
testVersion06

	| v1 v2 |
	v1 := self versionClass fromString: '3.'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)