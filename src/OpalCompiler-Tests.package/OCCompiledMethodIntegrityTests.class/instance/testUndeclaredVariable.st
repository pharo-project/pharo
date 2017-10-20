test
testUndeclaredVariable

	| newCompiledMethod undeclaredBinding |
	
	newCompiledMethod := OpalCompiler new 
								source: 'methodWithUndeclaredVar
											^ undeclaredTestVar';
								class: OCMockCompilationClass;
								compile.

	undeclaredBinding := newCompiledMethod 
		literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding class equals: UndeclaredVariable.
	self assert: undeclaredBinding == (Undeclared associationAt: #undeclaredTestVar).
	Undeclared removeKey: #undeclaredTestVar