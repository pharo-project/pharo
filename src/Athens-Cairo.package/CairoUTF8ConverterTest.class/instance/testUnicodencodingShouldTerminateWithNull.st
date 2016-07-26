tests
testUnicodencodingShouldTerminateWithNull
	| buf |
	buf := encoder convertChar: $a.
	
	
	self assert: (buf first:2)   equals: #[97 0 ]
