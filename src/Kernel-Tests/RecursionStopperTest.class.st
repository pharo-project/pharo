"
SUnit tests for class  RecursionStopper
"
Class {
	#name : #RecursionStopperTest,
	#superclass : #TestCase,
	#instVars : [
		'fork',
		'value'
	],
	#category : #'Kernel-Tests-Processes'
}

{ #category : #accessing }
RecursionStopperTest >> mixedMethod [

	RecursionStopper during: [ 
		self assert: value equals: 0.
		value := value + 1.
		RecursionStopper during: [ 
			value := value + 1.
		].
		self assert: value equals: 1.
		self mixedMethod.
		self assert: value equals: 1.		
	].
	
	RecursionStopper during: [ 
		self assert: value equals: 1.
		^ 2
	].

	self assert: value equals: 1.
	^ value 
]

{ #category : #accessing }
RecursionStopperTest >> recursion [
	
	RecursionStopper during: [ self recursion ].
]

{ #category : #running }
RecursionStopperTest >> setUp [
	super setUp.
	value := 0
]

{ #category : #running }
RecursionStopperTest >> tearDown [

	fork ifNotNil: [ fork terminate. fork := nil ].
	super tearDown
]

{ #category : #tests }
RecursionStopperTest >> testMixedMethod [

	| result |

	self should: [ result := self mixedMethod ] notTakeMoreThanMilliseconds: 3.
	
	self assert: result equals: 2
]

{ #category : #tests }
RecursionStopperTest >> testNoRecursion [

	RecursionStopper during: [ value := value + 1 ].

	self assert: value equals: 1
]

{ #category : #tests }
RecursionStopperTest >> testThreadSafe [

	self should: [ self threadSafe ] notTakeMoreThanMilliseconds: 10.
	
	self assert: fork isNil.
]

{ #category : #tests }
RecursionStopperTest >> testWithRecursion [

	self should: [ self recursion ] notTakeMoreThanMilliseconds: 3.
]

{ #category : #accessing }
RecursionStopperTest >> threadSafe [

	| sema |
	
	RecursionStopper during: [ 
		self assert: fork isNil.
		sema := Semaphore new.
		
		fork := [ self threadSafe. sema signal ] fork.	
		
		sema wait.
		self assert: fork isTerminated.
			
		fork := nil.
		
	].	

]
