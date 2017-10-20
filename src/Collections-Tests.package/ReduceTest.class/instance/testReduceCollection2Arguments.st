tests
testReduceCollection2Arguments
	| block |
	block := [ :a :b | a + b ].
	self collectionClasses do: [ :class |
		self should: [ class new reduce: block ] raise: Error.
		self assert: ((class with: 1) reduce: block) = 1.
		self assert: ((class with: 1 with: 2) reduce: block) = 3.
		self assert: ((class with: 1 with: 2 with: 3) reduce: block) = 6.
		self assert: ((class with: 1 with: 2 with: 3 with: 4) reduce: block) = 10 ]