tests-perform
testPerformAddRemoveClassMethod
	| change source |
	change := changes compile: 'method ^ 1' in: self changeMock class classified: #utilities.
	self perform: change do: [ self assert: (self changeMock respondsTo: #method) ].
	self deny: (self changeMock respondsTo: #method).
	self assert: change definedSelector = #method