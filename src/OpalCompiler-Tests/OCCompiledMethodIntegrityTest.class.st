Class {
	#name : #OCCompiledMethodIntegrityTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testBlockTemps [
	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new
		source:
			'ascentOf: aCharacter
											^ [ | temp1 temp2 temp3 | 1= temp1. 2 = temp2. 3 = temp3].';
		class: OCMockCompilationClass;
		compile.

	self assert: newCompiledMethod numArgs equals: 1.
	self assert: (newCompiledMethod numLiterals = 3 or: [ newCompiledMethod numLiterals = 4	"Sista Bytecode" ]).
	self assert: newCompiledMethod numTemps equals: 1.
	self assert: newCompiledMethod primitive equals: 0
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testNotUsedArgument [
	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new
		source:
			'ascentOf: aCharacter
											^ self ascent.';
		class: OCMockCompilationClass;
		compile.

	self assert: newCompiledMethod numArgs equals: 1.
	self assert: newCompiledMethod numLiterals equals: 3.
	self assert: newCompiledMethod numTemps equals: 1.
	self assert: newCompiledMethod primitive equals: 0
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testPragmas [
	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new
		source:
			'methodDoublePragma
											<hello: 5>
											<hello: 2>';
		class: OCMockCompilationClass;
		compile.

	self assert: newCompiledMethod numArgs equals: 0.
	self assert: newCompiledMethod numLiterals equals: 2.

	"AdditionalState assertions"
	self assert: newCompiledMethod allLiterals first selector equals: #methodDoublePragma.
	self assert: (newCompiledMethod allLiterals first instVarNamed: 'method') equals: newCompiledMethod.

	self assert: newCompiledMethod primitive equals: 256
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testPrimitive [
	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new
		source:
			'size
											<primitive: 62>
											^ self basicSize.';
		class: OCMockCompilationClass;
		compile.

	self assert: newCompiledMethod numArgs equals: 0.
	self assert: newCompiledMethod numLiterals equals: 3.
	self assert: newCompiledMethod numTemps equals: 0.
	self assert: newCompiledMethod primitive equals: 62
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testRemoteTempInVector [
	| newCompiledMethod expectedLiterals |
	newCompiledMethod := OpalCompiler new
		source:
			'value
													    | answer |
 													   self do:
       														 [:each |
        															answer := each value].
   													 ^answer';
		class: Object;
		compile.

	"In FullBlockClosure the block is another literal of the method"
	expectedLiterals := (CompilationContext optionFullBlockClosure) ifTrue: [ 3 ] ifFalse: [ 2 ].

	self assert: newCompiledMethod numArgs equals: 0.
	self assert: newCompiledMethod numLiterals equals: expectedLiterals.
	self assert: newCompiledMethod numTemps equals: 1.
	self assert: newCompiledMethod primitive equals: 0
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testUndeclaredVariable [
	| newCompiledMethod undeclaredBinding |
	newCompiledMethod := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		compile.

	undeclaredBinding := newCompiledMethod literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding class equals: UndeclaredVariable.
	self assert: undeclaredBinding identicalTo: (Undeclared associationAt: #undeclaredTestVar).
	undeclaredBinding unregister
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testUndeclaredVariableWhenItIsAlreadyRegistered [
	| newCompiledMethod undeclaredBinding var |
	var := UndeclaredVariable registeredWithName: #undeclaredTestVar.
	newCompiledMethod := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		compile.

	undeclaredBinding := newCompiledMethod literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding identicalTo: var.
	undeclaredBinding unregister
]

{ #category : #tests }
OCCompiledMethodIntegrityTest >> testUndeclaredVariableWhenItIsAlreadyRegisteredAsAssociation [
	"Test describes current state of bootstrap where undeclared vars are encoded using associations.
	It breaks introduced logic which explicitly return existing variable from Undeclared registry. 
	Compiler parts expect kind of variable and not a simple association.
	The bootstrap will be updated at some point. But now this case needs to be supported: 
		- declaring undefined var should update registered variable when its type is not undeclared var"
	| newCompiledMethod undeclaredBinding |
	Undeclared add: #undeclaredTestVar -> nil.
	newCompiledMethod := OpalCompiler new
		source:
			'methodWithUndeclaredVar
											^ undeclaredTestVar';
		class: OCMockCompilationClass;
		compile.

	undeclaredBinding := newCompiledMethod literals detect: [ :each | each name = #undeclaredTestVar ].
	self assert: undeclaredBinding class equals: UndeclaredVariable.
	self assert: undeclaredBinding identicalTo: (Undeclared associationAt: #undeclaredTestVar).
	undeclaredBinding unregister
]
