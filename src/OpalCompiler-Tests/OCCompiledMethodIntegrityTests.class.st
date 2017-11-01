Class {
	#name : #OCCompiledMethodIntegrityTests,
	#superclass : #TestCase,
	#category : 'OpalCompiler-Tests-Misc'
}

{ #category : #test }
OCCompiledMethodIntegrityTests >> testBlockTemps [

	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new 
								source: 'ascentOf: aCharacter
											^ [ | temp1 temp2 temp3 | 1= temp1. 2 = temp2. 3 = temp3].';
								class: OCMockCompilationClass;
								compile.
								
	self assert: newCompiledMethod numArgs = 1.
	self assert: (newCompiledMethod numLiterals = 3 or: [newCompiledMethod numLiterals = 4 "Sista Bytecode" ]).
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.
]

{ #category : #test }
OCCompiledMethodIntegrityTests >> testNotUsedArgument [

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
								source: 'ascentOf: aCharacter
											^ self ascent.';
								class: OCMockCompilationClass;
								compile.

	self assert: newCompiledMethod numArgs = 1.
	self assert: newCompiledMethod numLiterals = 3.
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.
]

{ #category : #test }
OCCompiledMethodIntegrityTests >> testPragmas [

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
									source: 'methodDoublePragma
											<hello: 5>
											<hello: 2>';
									class: OCMockCompilationClass;
									compile.
									
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 2.
	
	"AdditionalState assertions"
	self assert: newCompiledMethod literals first selector = #methodDoublePragma.
	self assert: (newCompiledMethod literals first instVarNamed: 'method') = newCompiledMethod.
		
	self assert: newCompiledMethod primitive = 256.
]

{ #category : #test }
OCCompiledMethodIntegrityTests >> testPrimitive [

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
								source: 'size
											<primitive: 62>
											^ self basicSize.';
								class: OCMockCompilationClass;
								compile.
							
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 3.
	self assert: newCompiledMethod numTemps = 0.
	self assert: newCompiledMethod primitive = 62.
]

{ #category : #test }
OCCompiledMethodIntegrityTests >> testRemoteTempInVector [

	| newCompiledMethod |

	"Here the problem was that the Scope kept both the remote temp answer and the new remote <?> this caused
	that the number of temps were more than the correnct"
	
	newCompiledMethod := OpalCompiler new 
											source: 'value
													    | answer |
 													   self do:
       														 [:each |
        															answer := each value].
   													 ^answer';
											class: Object;
											compile.

							
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 2.
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.
]

{ #category : #test }
OCCompiledMethodIntegrityTests >> testUndeclaredVariable [

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
]
