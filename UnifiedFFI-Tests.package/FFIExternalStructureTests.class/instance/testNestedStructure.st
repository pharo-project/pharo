tests
testNestedStructure
	| s1 |

	s1 := FFITestNestingStructure new.
	s1 nested byte: 42.
	self assert: s1 nested byte = 42