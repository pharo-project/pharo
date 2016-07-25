tests-perform
testPerformAddRemoveMethod
	| change source |
	change := changes compile: 'method ^ 1' in: self changeMock classified: #utilities.
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector = #method