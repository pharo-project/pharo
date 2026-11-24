"
I test `ReExampleShouldBeSampleInstanceRule`: which class-side `example` methods are reported as sample instances without the pragma `<sampleInstance>`.

## Responsibility:

- Check that each way of returning an instance is reported: `new`, `super new`, `basicNew`, `instance creation` methods, cascades ending with `yourself`, temporaries, assignments, the class named directly.
- Check that other returns, instance-side methods, other selectors and methods with the pragma are not reported.

## Collaborators:

- `ReAbstractSampleInstanceRuleTest`: compiles the methods and answers the critiques.

```
self assert: (self critiquesOfClassMethod: 'exampleFoo <sampleInstance> ^ self new') size equals: 0
```
"
Class {
	#name : 'ReExampleShouldBeSampleInstanceRuleTest',
	#superclass : 'ReAbstractSampleInstanceRuleTest',
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleNamingAnotherClassIsNotReported [

	| other |
	other := classFactory make: [ :aBuilder | aBuilder ].
	self assert: (self critiquesOfClassMethodNamed: #exampleFoo source: [ :class | 'exampleFoo ^ {1} new' format: { other name } ]) size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleNamingItsClassIsReported [

	self assert: (self critiquesOfClassMethodNamed: #exampleFoo source: [ :class | 'exampleFoo ^ {1} new' format: { class name } ]) size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningAssignmentOfNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo | foo | ^ foo := self new') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningBasicNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self basicNew') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningCascadeEndingWithYourselfIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new printString; yourself') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningCascadeNotEndingWithYourselfIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new printString; size') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningInstanceCreationMethodIsReported [

	self
		assert: (self critiquesOfClassMethod: 'exampleFoo ^ self named: 1' with: { #'instance creation' -> 'named: x ^ self new' }) size
		equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningMessageToNewIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new printString') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningOtherClassMethodIsNotReported [

	self
		assert: (self critiquesOfClassMethod: 'exampleFoo ^ self named: 1' with: { #helpers -> 'named: x ^ self new' }) size
		equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningOtherTemporaryIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo | foo bar | foo := self new. bar := 42. ^ bar') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningSelfIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo self new printString') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningSuperNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ super new') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningTemporaryAssignedFromItselfIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo | foo | foo := foo yourself. ^ foo') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningTemporaryHoldingNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo | foo | foo := self new. ^ foo') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningTemporaryHoldingOtherObjectIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo | foo | foo := self new printString. ^ foo') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleReturningYourselfOfNewIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new yourself') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleWithOtherPragmaIsReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo <example> ^ self new') size equals: 1
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testExampleWithSampleInstanceIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo <sampleInstance> ^ self new') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testInstanceSideExampleIsNotReported [

	self assert: (self critiquesOfInstanceMethod: 'exampleFoo ^ self new') size equals: 0
]

{ #category : 'tests' }
ReExampleShouldBeSampleInstanceRuleTest >> testMethodNotNamedExampleIsNotReported [

	self assert: (self critiquesOfClassMethod: 'fooExample ^ self new') size equals: 0
]
