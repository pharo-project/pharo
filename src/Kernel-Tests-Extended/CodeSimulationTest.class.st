"
SUnit tests for code simulation
"
Class {
	#name : #CodeSimulationTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #private }
CodeSimulationTest >> indexedBasicAt: index [ 
	<primitive: 60 error: code >
	^ code


]

{ #category : #tests }
CodeSimulationTest >> methodWithError [
	self error: 'my error'
]

{ #category : #tests }
CodeSimulationTest >> methodWithTranscript [
	self trace: 'something'
]

{ #category : #tests }
CodeSimulationTest >> runSimulated: aBlock [
	thisContext runSimulated: aBlock contextAtEachStep: [ :current |  ]
]

{ #category : #tests }
CodeSimulationTest >> testDNU [
	self should: [ self runSimulated: [ self absentMethod ] ] raise: MessageNotUnderstood
]

{ #category : #tests }
CodeSimulationTest >> testError [
	self should: [ self runSimulated: [self methodWithError] ] raise: Error
]

{ #category : #'tests - primitives' }
CodeSimulationTest >> testErrorCodeNotFound [
	| ctx result resultSimu |
	self skip.

	Smalltalk vm isRunningCog ifFalse: [ ^ self ].

	result := self veryBasicAt: 1.

	ctx := Context
		sender: nil
		receiver: nil
		method: Object >> #at:
		arguments: #(10).

	resultSimu := ctx
		push: nil;
		push: 500;
		doPrimitive: 117
			method: self class >> #veryBasicAt:
			receiver: self
			args: #(999).

	self assert: resultSimu isArray.
	self assert: Context primitiveFailToken first identicalTo: resultSimu first.
	self assert: result equals: resultSimu second
]

{ #category : #'tests - primitives' }
CodeSimulationTest >> testErrorCodeNotFoundIndexed [
	| ctx result resultSimu |
	Smalltalk vm isRunningCog ifFalse: [ ^ self ].

	result := self indexedBasicAt: 100.

	ctx := Context
		sender: nil
		receiver: nil
		method: Object >> #at:
		arguments: #(10).

	resultSimu := ctx
		push: nil;
		push: 500;
		doPrimitive: 60
			method: self class >> #indexedBasicAt:
			receiver: self
			args: #(100).

	self assert: resultSimu isArray.
	self assert: resultSimu size equals: 2.
	self assert: Context primitiveFailToken first identicalTo: resultSimu first.
	self assert: result equals: resultSimu second
]

{ #category : #'tests - primitives' }
CodeSimulationTest >> testErrorToken [
	| token1 token2 |
	token1 := Context primitiveFailToken.
	token2 := Context primitiveFailTokenFor: 100.

	self assert: token1 first identicalTo: token2 first.
	self assert: token1 second isNil.
	self assert: token2 second equals: 100
]

{ #category : #tests }
CodeSimulationTest >> testErrorWithErrorHandler [
	self
		runSimulated: [ [ self methodWithError ]
				on: Error
				do: [ :err |  ] ]
]

{ #category : #tests }
CodeSimulationTest >> testGoodSimulation [
	self runSimulated: [ 1 + 2 ].
]

{ #category : #tests }
CodeSimulationTest >> testTranscriptPrinting [
	self runSimulated: [ self methodWithTranscript ]
]

{ #category : #tests }
CodeSimulationTest >> testTranscriptPrintingWithOpenedTranscriptExists [
	self runSimulated: [ self methodWithTranscript ]
]

{ #category : #private }
CodeSimulationTest >> veryBasicAt: index [ 
	<primitive: 'dooo' module: 'bar' error: code>
	^ code

]
