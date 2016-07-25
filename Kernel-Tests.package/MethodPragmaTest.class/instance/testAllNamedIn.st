testing-finding
testAllNamedIn
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: in: self class.
	self assert: pragmasDetected = pragmasCompiled.
	
	pragmasDetected := Pragma allNamed: #foo: in: Object.
	self assert: pragmasDetected isEmpty.