Class {
	#name : 'OCUndeclaredTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'tests' }
OCUndeclaredTest >> testUndeclaredVariable [
	| cm undeclaredBinding |
	cm := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		permitFaulty: true;
		compileOrFail.

	undeclaredBinding := cm literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding class equals: UndeclaredVariable.
	self assert: undeclaredBinding identicalTo: (Undeclared associationAt: #undeclaredTestVar).
	self assert: (undeclaredBinding registeredMethods includes: cm).
	"but as the method is not installed, it is not a usingMethod"
	self deny: (undeclaredBinding usingMethods includes: cm).
	undeclaredBinding unregister
]

{ #category : 'tests' }
OCUndeclaredTest >> testUndeclaredVariableWhenItIsAlreadyRegistered [
	| cm undeclaredBinding var |
	var := UndeclaredVariable registeredWithName: #undeclaredTestVar.
	cm := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		permitFaulty: true;
		compileOrFail.

	undeclaredBinding := cm literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding identicalTo: var.
	undeclaredBinding unregister
]

{ #category : 'tests' }
OCUndeclaredTest >> testUndeclaredVariableWhenItIsAlreadyRegisteredAsAssociation [
	"Test describes current state of bootstrap where undeclared vars are encoded using associations.
	It breaks introduced logic which explicitly return existing variable from Undeclared registry.
	Compiler parts expect kind of variable and not a simple association.
	The bootstrap will be updated at some point. But now this case needs to be supported:
		- declaring undefined var should update registered variable when its type is not undeclared var"
	| newCompiledMethod undeclaredBinding |
	(UndeclaredVariable named: #undeclaredTestVar) register.
	newCompiledMethod := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		permitFaulty: true;
		compileOrFail.

	undeclaredBinding := newCompiledMethod literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding class equals: UndeclaredVariable.
	self assert: undeclaredBinding identicalTo: (Undeclared associationAt: #undeclaredTestVar).
	undeclaredBinding unregister
]

{ #category : 'tests' }
OCUndeclaredTest >> testUndefinedVariable [
	"This test shows that undefined variables behave as globals (for now) and is independent of the compilation mode"

	Undeclared removeKey: #undefinedName123 ifAbsent: [ ].
	self
		assert: ( [ {
			OpalEvaluator new
				permitFaulty: true;
				evaluate: 'undefinedName123'.
			OpalEvaluator new
				permitFaulty: true;
				evaluate: 'undefinedName123 := 1'.
			OpalEvaluator new
				permitFaulty: true;
				evaluate: 'undefinedName123 := 2. undefinedName123'.
			OpalEvaluator new
				permitFaulty: true;
				evaluate: 'undefinedName123' }  ] on: UndeclaredVariableRead , UndeclaredVariableWrite do: [ :e | e resume: e class ] )
		equals: { UndeclaredVariableRead. UndeclaredVariableWrite. UndeclaredVariableRead. UndeclaredVariableRead }.

	"Cleanup"
	Undeclared removeKey: #undefinedName123 ifAbsent: [ ]
]
