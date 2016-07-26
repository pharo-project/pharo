tests-perform
testPerformChangeMethod
	| change source |
	change := changes compile: 'one ^ 2' in: self changeMock.
	source := change changeClass sourceCodeAt: #one.
	self perform: change do: [ self assert: (change changeClass sourceCodeAt: #one) = 'one ^ 2' ].
	self assert: (change changeClass sourceCodeAt: #one) = source