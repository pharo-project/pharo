tests-perform
testPerformAddRemoveMethodInteractively
	| change source |
	change := RBAddMethodChange compile: 'method ^ 1' in: self changeMock classified: #utilities for: self. 
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector = #method