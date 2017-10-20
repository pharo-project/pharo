tests
testReduceLeft3Arguments
	| block |
	block := [ :a :b :c | Array with: a with: b with: c ].
	self sequenceableClasses do: [ :class |
		self should: [ class new reduceLeft: block ] raise: Error.
		self assert: ((class with: #a) reduceLeft: block) = #a.
		self should: [ (class with: #a with: #b) reduceLeft: block ] raise: Error.
		self assert: ((class with: #a with: #b with: #c) reduceLeft: block) = #(a b c).
		self should: [ (class with: #a with: #b with: #c with: #d) reduceLeft: block ] raise: Error.
		self assert: ((class with: #a with: #b with: #c with: #d with: #e) reduceLeft: block) = #((a b c) d e) ]