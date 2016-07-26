testing-finding
testAllNamedInSortedByArgument
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: in: self class sortedByArgument: 1.
	self assert: pragmasDetected = (pragmasCompiled 
		sort: [ :a :b | (a argumentAt: 1) < (b argumentAt: 1) ])