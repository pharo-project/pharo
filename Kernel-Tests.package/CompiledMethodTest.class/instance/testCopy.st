tests - comparing
testCopy
	<pragma: #pragma>
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method copy.
	self assert: (method equivalentTo: copy).
	self assert: method header = copy header.
	self assert: method = copy.
	self assert: method ~~ copy.
	self assert: copy penultimateLiteral method == copy.
	self assert: method penultimateLiteral method == method.
	method pragmas do:
		[:p|
		self assert: p method == method].
	copy pragmas do:
		[:p|
		self assert: p method == copy]
