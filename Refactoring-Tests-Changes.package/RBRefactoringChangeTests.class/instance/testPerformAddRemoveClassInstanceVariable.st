tests-perform
testPerformAddRemoveClassInstanceVariable
	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMock class.
	self perform: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')