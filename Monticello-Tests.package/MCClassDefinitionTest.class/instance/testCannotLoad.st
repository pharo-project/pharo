tests
testCannotLoad
	| d |
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').
	d :=  self mockClass: 'MCMockClassC' super: 'NotAnObject'.
	self should: [d load] raise: Error.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC').