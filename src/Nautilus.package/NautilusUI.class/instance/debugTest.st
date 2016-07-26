debugging actions
debugTest
	| method |
	method := self selectedMethod.
	method correspondingTestMethod 
		ifNotNil: [:test | 
			self debugTest: test ]