"
SUnit tests for code simulation
"
Class {
	#name : #CodeSimulationTests,
	#superclass : #TestCase,
	#classVars : [
		'log'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #private }
CodeSimulationTests >> indexedBasicAt: index [ 
	<primitive: 60 error: code >
	^ code


]

{ #category : #tests }
CodeSimulationTests >> methodWithError [
	self error: 'my error'
]

{ #category : #tests }
CodeSimulationTests >> methodWithHalt [
	self halt
]

{ #category : #tests }
CodeSimulationTests >> methodWithTranscript [
	Transcript show: 'something'
]

{ #category : #tests }
CodeSimulationTests >> runSimulated: aBlock [
	thisContext runSimulated: aBlock contextAtEachStep: [ :current |  ]
]

{ #category : #tests }
CodeSimulationTests >> testDNU [

	self should: [ self runSimulated: [self absentMethod] ] raise: MessageNotUnderstood
]

{ #category : #tests }
CodeSimulationTests >> testError [
	self should: [ self runSimulated: [self methodWithError] ] raise: Error
]

{ #category : #'tests - primitives' }
CodeSimulationTests >> testErrorCodeNotFound [
	| ctx result resultSimu |
	
	self skip.
	
	Smalltalk vm isRunningCog ifFalse: [^self].
	
	result := self veryBasicAt: 1.
		
	ctx := Context 
		sender: nil
		receiver: nil
		method: (Object>>#at: )
		arguments: #(10).
	
	resultSimu := ctx 
		push: nil;
		push: 500;
		doPrimitive: 117  method:  (self class>>#veryBasicAt:) receiver: self args: #(999).
		
	self assert: resultSimu isArray.	
	self assert: Context  primitiveFailToken first == resultSimu first.
	self assert: result = resultSimu second.
]

{ #category : #'tests - primitives' }
CodeSimulationTests >> testErrorCodeNotFoundIndexed [
	| ctx result resultSimu |
	
	Smalltalk vm isRunningCog ifFalse: [^self].
	
	result := self indexedBasicAt: 100.
		
	ctx := Context 
		sender: nil
		receiver: nil
		method: (Object>>#at: )
		arguments: #(10).
	
	resultSimu := ctx 
		push: nil;
		push: 500;
		doPrimitive: 60  method:  (self class>>#indexedBasicAt:) receiver: self args: #(100).
		
	self assert: resultSimu isArray.	
	self assert: resultSimu size = 2.
	self assert: Context  primitiveFailToken first == resultSimu first.
	self assert: result = resultSimu second.
]

{ #category : #'tests - primitives' }
CodeSimulationTests >> testErrorToken [
	| token1 token2 |
	
	token1 := Context primitiveFailToken.
	token2 := Context primitiveFailTokenFor: 100.

	self assert: token1 first == token2 first.
	self assert: token1 second == nil.
	self assert: token2 second == 100.
]

{ #category : #tests }
CodeSimulationTests >> testErrorWithErrorHandler [
	self runSimulated: [[self methodWithError] on: Error do: [:err | ]]  
]

{ #category : #tests }
CodeSimulationTests >> testGoodSimulation [
	self runSimulated: [ 1 + 2 ].
]

{ #category : #tests }
CodeSimulationTests >> testHalt [
	self should: [ self runSimulated: [self methodWithHalt] ] raise: Halt
]

{ #category : #tests }
CodeSimulationTests >> testHaltWithHaltHandler [
	self runSimulated: [[self methodWithHalt] on: Halt do: [:err |]]  
]

{ #category : #tests }
CodeSimulationTests >> testTranscriptPrinting [
	self runSimulated: [self methodWithTranscript]  
]

{ #category : #tests }
CodeSimulationTests >> testTranscriptPrintingWithOpenedTranscriptExists [
	self runSimulated: [self methodWithTranscript]  
]

{ #category : #private }
CodeSimulationTests >> veryBasicAt: index [ 
	<primitive: 'dooo' module: 'bar' error: code>
	^ code

]
