"
SUnit tests for class  LocalRecursionStopper
"
Class {
	#name : #LocalRecursionStopperTest,
	#superclass : #TestCase,
	#instVars : [
		'fork',
		'value'
	],
	#category : #'Kernel-Tests-Processes'
}

{ #category : #accessing }
LocalRecursionStopperTest >> complexRecursion [
	
	LocalRecursionStopper during: [ self complexRecursionFirstCall ].
]

{ #category : #accessing }
LocalRecursionStopperTest >> complexRecursionFirstCall [

	LocalRecursionStopper during: [ self complexRecursionLastCall ]
]

{ #category : #accessing }
LocalRecursionStopperTest >> complexRecursionLastCall [
	
	self complexRecursion
]

{ #category : #accessing }
LocalRecursionStopperTest >> mixedMethod [

	LocalRecursionStopper during: [ 
		self assert: value equals: 0.
		value := value + 1.
		LocalRecursionStopper during: [ 
			value := value + 1.
		].
		self assert: value equals: 1.
		self mixedMethod.
		self assert: value equals: 1.		
	].
	
	LocalRecursionStopper during: [ 
		self assert: value equals: 1.
		^ 2
	].

	self assert: value equals: 1.
	^ value 
]

{ #category : #accessing }
LocalRecursionStopperTest >> recursion [
	
	LocalRecursionStopper during: [ self recursion ].
]

{ #category : #running }
LocalRecursionStopperTest >> setUp [
	super setUp.
	value := 0
]

{ #category : #running }
LocalRecursionStopperTest >> tearDown [

	fork ifNotNil: [ fork terminate. fork := nil ].
	super tearDown 
]

{ #category : #tests }
LocalRecursionStopperTest >> testMixedMethod [

	| result |

	self should: [ result := self mixedMethod ] notTakeMoreThanMilliseconds: 3.
	
	self assert: result equals: 2
]

{ #category : #tests }
LocalRecursionStopperTest >> testNoRecursion [

	LocalRecursionStopper during: [ value := value + 1 ].

	self assert: value equals: 1
]

{ #category : #tests }
LocalRecursionStopperTest >> testWithComplexRecursion [

	self should: [ self complexRecursion ] notTakeMoreThanMilliseconds: 3.
]

{ #category : #tests }
LocalRecursionStopperTest >> testWithRecursion [

	self should: [ self recursion ] notTakeMoreThanMilliseconds: 3.
]
