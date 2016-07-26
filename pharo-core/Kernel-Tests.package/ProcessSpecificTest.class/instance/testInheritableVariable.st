testing
testInheritableVariable

	| s1 s2 parentVar childVar |
	s1 := Semaphore new.
	s2 := Semaphore new.
	TestInheritableVariable enable.
	
	[ TestInheritableVariable value: #inheritedVar.
		parentVar := TestInheritableVariable value.
		[ childVar := TestInheritableVariable value. s2 signal ] fork.
		s1 signal.
	 ] fork.

	s1 waitTimeoutSeconds: 2.
	s2 waitTimeoutSeconds: 2.
	TestInheritableVariable disable.
	
	self assert: parentVar equals: #inheritedVar.
	self assert: childVar equals: #inheritedVar