testing
testIsClean
	 | local |
	 local := #testIsClean.
	self assert: [] isClean. "closes over nothing at all"
	self assert: [:a :b| a < b] isClean. "accesses only arguments"
	self assert: [:a :b| | s | s := a + b. s even] isClean. "accesses only local variables"
	self deny: [^nil] isClean. "closes over home (^-return)"
	self deny: [self] isClean. "closes over the receiver"
	self deny: [collection] isClean. "closes over the receiver (to access the inst var collection)"
	self deny: [local] isClean. "closes over local variable of outer context"