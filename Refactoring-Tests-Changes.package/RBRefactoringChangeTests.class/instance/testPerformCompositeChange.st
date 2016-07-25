tests-perform
testPerformCompositeChange
	changes 
		compile: 'method1 ^ 1' in: self changeMock;
		compile: 'method2 ^ 2' in: self changeMock.
	self perform: changes do: [ 
		self assert: (self changeMock canUnderstand: #method1).
		self assert: (self changeMock canUnderstand: #method2) ].
	self deny: (self changeMock canUnderstand: #method1).
	self deny: (self changeMock canUnderstand: #method2) 