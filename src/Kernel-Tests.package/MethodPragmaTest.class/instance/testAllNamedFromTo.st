testing-finding
testAllNamedFromTo
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: from: self class to: Object.
	self assert: pragmasDetected = pragmasCompiled.
	
	pragmasDetected := Pragma allNamed: #foo: from: Object to: Object.
	self assert: pragmasDetected isEmpty.