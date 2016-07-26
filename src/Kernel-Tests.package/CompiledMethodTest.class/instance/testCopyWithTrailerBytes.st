tests - comparing
testCopyWithTrailerBytes
	<pragma: #pragma>
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method copyWithTrailerBytes: method trailer.
	self assert: (method equivalentTo: copy).
	self deny: method == copy.
	self assert: method symbolic = copy symbolic. "but their bytecode should be the same"
	self assert: method ~~ copy.
	self assert: copy penultimateLiteral method == copy.
	self assert: method penultimateLiteral method == method.
	method pragmas do:
		[:p|
		self assert: p method == method].
	copy pragmas do:
		[:p|
		self assert: p method == copy]
