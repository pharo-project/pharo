Class {
	#name : 'ContextDebuggingInfoTest',
	#superclass : 'TestCase',
	#instVars : [
		'instVarForTestLookupSymbol'
	],
	#classVars : [
		'classVarForTestLookupSymbol'
	],
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'closures for testing' }
ContextDebuggingInfoTest class >> contextWithTempForTesting [
	| string |
	string := 'test'.
	^ [self class. string asUppercase] asContext
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testReadVariableNamed [
	|localVar|
	localVar := 2.
	instVarForTestLookupSymbol := 3.
	classVarForTestLookupSymbol := 4.
	self assert: (thisContext readVariableNamed: #localVar) equals: 2.
	self assert: (thisContext readVariableNamed: #instVarForTestLookupSymbol) equals: 3.
	self assert: (thisContext readVariableNamed: #classVarForTestLookupSymbol) equals: 4.
	self assert: (thisContext readVariableNamed: #Smalltalk) equals: Smalltalk
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testScopeOptimizedBlock [
	"When asking for the scope of a block that is not there at runtime, we nevertheless what to
	get the corresponding scope of the optimized block"

	| astScope method |
	method := Object compiler
		          source: 'test true ifTrue: [ ^ thisContext astScope ]';
		          compileOrFail.
	astScope := method valueWithReceiver: nil.

	self assert: astScope class equals: OCOptimizedBlockScope
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testSourceNodeExecuted [
	| sourceNode |
	sourceNode := thisContext sender sender sourceNodeExecuted.
	self assert: sourceNode selector equals: #performTest
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testSourceNodeExecutedWhenContextIsJustAtStartpc [
	| sourceNode context |

	context := Context sender: thisContext receiver: self method: self class >> testSelector arguments: #().
	sourceNode := context sourceNodeExecuted.
	self assert: sourceNode equals: (self class >> testSelector) ast sendNodes first receiver
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testSourceNodeOptimizedBlock [
	"When asking for the sourceNode of a block that is not there at runtime, we nevertheless what to
	get the corresponding ASTnode of the ASTNode that created thisContext, the method in this case"

	| sourceNode method |
	method := Object compiler
		          source: 'test 1 to: 1 do: [ :i | ^ thisContext sourceNode ]';
		          compileOrFail.
	sourceNode := method valueWithReceiver: nil.

	self assert: sourceNode isMethod
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamed [
	| oneTemp context |
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp') equals: oneTemp.

	context := self class contextWithTempForTesting.
	self assert: (context tempNamed: 'string') equals: 'test'
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamedAtTheEndOfOptimizedBlock [
	
	| s process |
	s := Semaphore new.
	process := [ 1 to: 17 do: [ :i | 
			s signal.
			Processor activeProcess suspend ] ] fork.
	s wait.
	
	self assert: (process suspendedContext temporaryVariableNames includes: #i)
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamedPut [
	| oneTemp |
	oneTemp := 1.
	thisContext tempNamed: 'oneTemp' put: 2.

	self assert: (thisContext tempNamed: 'oneTemp') equals: 2
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamedPutShouldFailGivenNameIsNotTemp [

	self assert: (self class hasSlotNamed: #expectedFails).
	self should: [thisContext tempNamed: #expectedFails put: 0] raise: Error
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamedPutShouldReturnAssignedValue [
	| oneTemp |
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp' put: 2) equals: 2
]

{ #category : 'tests' }
ContextDebuggingInfoTest >> testTempNamedShouldFailGivenNameIsNotTemp [

	self assert: (self class hasSlotNamed: #testSelector).
	self should: [thisContext tempNamed: #testSelector] raise: Error
]
