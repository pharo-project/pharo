tests
testReduceLeft2Arguments
	| block |
	block := [ :a :b | Array with: a with: b ].
	self sequenceableClasses do: [ :class |
		self should: [ class new reduceLeft: block ] raise: Error.
		self assert: ((class with: #a) reduceLeft: block) = #a.
		self assert: ((class with: #a with: #b) reduceLeft: block) = #(a b).
		self assert: ((class with: #a with: #b with: #c) reduceLeft: block) = #((a b) c).
		self assert: ((class with: #a with: #b with: #c with: #d) reduceLeft: block) = #(((a b) c) d).
		self assert: ((class with: #a with: #b with: #c with: #d with: #e) reduceLeft: block) = #((((a b) c) d) e) ]