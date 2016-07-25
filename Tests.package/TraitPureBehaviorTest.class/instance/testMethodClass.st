testing-applying trait composition
testMethodClass
	"Test sharing of compiled methods between traits and their users. Methods are installed in exactly one behavior, however, the source pointers of methods are shared (unless sources or changes have been condensed). Verify	that methodClass properties are set correctly."
	
	"self debug: #testMethodClass"

	| m1 m2 |
	m1 := self t5 >> #m51.
	m2 := self c2 >> #m51.
	
	self assert: m1 methodClass == self t5.
	self assert: m2 methodClass == self c2.
	
	self deny: m1 == m2.
	
	self deny: m1 sourcePointer = m2 sourcePointer.