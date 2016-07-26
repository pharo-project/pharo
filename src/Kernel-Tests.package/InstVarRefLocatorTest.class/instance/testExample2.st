tests
testExample2
	| method |

	method := self class compiledMethodAt: #example2.
	self deny: (self hasInstVarRef: method).