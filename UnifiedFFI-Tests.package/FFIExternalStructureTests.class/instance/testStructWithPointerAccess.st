tests
testStructWithPointerAccess
	| s1 s2 |	

	s1 := FFITestPointerStructure new.
	self assert: s1 getHandle isExternalAddress not.
	self assert: s1 pointer isNull.
	self assert: s1 nestedPointer isNull.
	
	s2 := FFITestStructure externalNew.
	s2 long: 42.
	s1 nestedPointer: s2.
	self deny: s1 nestedPointer isNull.
	self assert: s1 nestedPointer long = 42.