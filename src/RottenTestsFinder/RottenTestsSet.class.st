"
I am the datastructure holding rotten tests found during an analysis.
"
Class {
	#name : 'RottenTestsSet',
	#superclass : 'Object',
	#instVars : [
		'rottenTests',
		'testsVisitedCount',
		'testsRunCount'
	],
	#category : 'RottenTestsFinder-Results',
	#package : 'RottenTestsFinder',
	#tag : 'Results'
}

{ #category : 'visiting' }
RottenTestsSet >> acceptVisitor: aRTFResultsVisitor [
	^ aRTFResultsVisitor visitRottenTestsSet: self
]

{ #category : 'adding' }
RottenTestsSet >> add: aRottenTest [
	"Dispatching on RottenTest and subclasses because the way it is added depend on the result type."
	aRottenTest addYourselfIn: self rottenTests
]

{ #category : 'adding' }
RottenTestsSet >> addAll: aCollection [
	aCollection do: [ :rottenTest |
		self add: rottenTest ]
]

{ #category : 'adding' }
RottenTestsSet >> addCompiledMethod: aCompiledMethod [
	aCompiledMethod origin isTrait
		ifFalse: [ self add: (RottenTest compiledMethod: aCompiledMethod) ]
		ifTrue: [ self add: (RottenTestImplementedInTrait compiledMethod: aCompiledMethod) ]
]

{ #category : 'accessing-computed' }
RottenTestsSet >> allRottenCompiledMethods [
	"Returns all tests (compiled methods) found to be rotten. That is to say, if the test is implemented in a Trait, the method of each user in which the test is found to be rotten is reported."
	^ self rottenTests flatCollect: [ :rottenTest |
		rottenTest allRottenCompiledMethods ]
]

{ #category : 'initialization' }
RottenTestsSet >> initialize [
	super initialize.
	rottenTests := Set new
]

{ #category : 'testing' }
RottenTestsSet >> isCompiledMethodRotten: aCompiledMethod [
	^ self rottenTests anySatisfy: [ :rottenTest |
		rottenTest compiledMethod = aCompiledMethod ]
]

{ #category : 'accessing-computed' }
RottenTestsSet >> rottenCompiledMethods [
	"Returns tests (compiled methods) found to be rotten."
	^ self rottenTests collect: #compiledMethod
]

{ #category : 'accessing' }
RottenTestsSet >> rottenTestForCompiledMethod: aCompiledMethod ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock [
	^ self rottenTests
		detect: [ :rottenTest | rottenTest compiledMethod = aCompiledMethod ]
		ifFound: ifPresentBlock
		ifNone: ifAbsentBlock
]

{ #category : 'accessing' }
RottenTestsSet >> rottenTests [
	^ rottenTests
]

{ #category : 'accessing' }
RottenTestsSet >> testsRunCount: anObject [
	testsRunCount := anObject
]

{ #category : 'accessing' }
RottenTestsSet >> testsVisitedCount [

	^ testsVisitedCount ifNil: [ testsVisitedCount := 0 ]
]

{ #category : 'accessing' }
RottenTestsSet >> testsVisitedCount: anObject [
	testsVisitedCount := anObject
]
