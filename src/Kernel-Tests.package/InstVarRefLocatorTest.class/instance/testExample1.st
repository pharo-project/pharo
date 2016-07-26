tests
testExample1
	| method |

	method := self class compiledMethodAt: #example1.
	self assert: (self hasInstVarRef: method).