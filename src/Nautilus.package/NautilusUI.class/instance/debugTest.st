debugging actions
debugTest
	| method |
	method := self selectedMethod.
	method asTestMethod 
		ifNotNil: [:testMethod | 
			self debugTest: testMethod ]