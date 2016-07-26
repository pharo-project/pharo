tests-perform
testPerformAddRemoveClassVariable
	| change |
	change := changes addClassVariable: 'Foo' to: self changeMock.
	self perform: change do: [ self assert: (change changeClass classVarNames includes: 'Foo') ].
	self deny: (change changeClass classVarNames includes: 'Foo')