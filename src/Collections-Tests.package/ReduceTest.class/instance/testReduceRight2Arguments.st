tests
testReduceRight2Arguments
	| block |
	block := [ :a :b | Array with: a with: b ].
	self sequenceableClasses do: [ :class |
		self should: [ class new reduceRight: block ] raise: Error.
		self assert: ((class with: #a) reduceRight: block) = #a.
		self assert: ((class with: #a with: #b) reduceRight: block) = #(a b).
		self assert: ((class with: #a with: #b with: #c) reduceRight: block) = #(a (b c)).
		self assert: ((class with: #a with: #b with: #c with: #d) reduceRight: block) = #(a (b (c d))).
		self assert: ((class with: #a with: #b with: #c with: #d with: #e) reduceRight: block) = #(a (b (c (d e)))) ]