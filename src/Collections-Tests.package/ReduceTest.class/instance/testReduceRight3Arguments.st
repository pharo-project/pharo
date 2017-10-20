tests
testReduceRight3Arguments
	| block |
	block := [ :a :b :c | Array with: a with: b with: c ].
	self sequenceableClasses do: [ :class |
		self should: [ class new reduceRight: block ] raise: Error.
		self assert: ((class with: #a) reduceRight: block) = #a.
		self should: [ (class with: #a with: #b) reduceRight: block ] raise: Error.
		self assert: ((class with: #a with: #b with: #c) reduceRight: block) = #(a b c).
		self should: [ (class with: #a with: #b with: #c with: #d) reduceRight: block ] raise: Error.
		self assert: ((class with: #a with: #b with: #c with: #d with: #e) reduceRight: block) = #(a b (c d e)) ]