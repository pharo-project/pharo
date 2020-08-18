Extension { #name : #ContextTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testScopeOptimizedBlock [
	"When asking for the scope of a block that is not there at runtime, we nevertheless what to 
	get the corresponding scope of the optimized block"
	
	| astScope method |
	
	method := Smalltalk compiler
							  class: Object;
							  compile:  'test 1 to: 1 do: [ :i | ^ thisContext astScope ]'.
	astScope := method valueWithReceiver: nil arguments: #(). 
	
	self assert: astScope class equals: OCOptimizedBlockScope .
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testSourceNodeOptimizedBlock [
	"When asking for the sourceNode of a block that is not there at runtime, we nevertheless what to 
	get the corresponding ASTnode of the ASTNode that created thisContext, the method in this case"
	
	| sourceNode method |
	
	method := Smalltalk compiler
							  class: Object;
							  compile:  'test 1 to: 1 do: [ :i | ^ thisContext sourceNode ]'.
	sourceNode := method valueWithReceiver: nil arguments: #(). 
	
	self assert: sourceNode isMethod.
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testTempNamed [
	| oneTemp context |
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp') equals: oneTemp.

	context := self class contextWithTempForTesting.
	self assert: (context tempNamed: 'string') equals: 'test'
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testTempNamedPut [
	| oneTemp |
	oneTemp := 1.
	thisContext tempNamed: 'oneTemp' put: 2.

	self assert: (thisContext tempNamed: 'oneTemp') equals: 2
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testTempNamedPutShouldFailGivenNameIsNotTemp [
	
	self assert: (self class hasSlotNamed: #expectedFails).	
	self should: [thisContext tempNamed: #expectedFails put: 0] raise: Error
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testTempNamedPutShouldReturnAssignedValue [
	| oneTemp |
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp' put: 2) equals: 2
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ContextTest >> testTempNamedShouldFailGivenNameIsNotTemp [

	self assert: (self class hasSlotNamed: #testSelector).
	self should: [thisContext tempNamed: #testSelector] raise: Error
]
