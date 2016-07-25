tests
testExternallyAllocatedNestedStructure
	| s1 |
	
	s1 := FFITestNestingStructure externalNew.
	self assert: s1 getHandle isExternalAddress.
	s1 nested byte: 42.	
	self assert: s1 nested byte = 42