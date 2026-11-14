"
I test `ReSampleInstanceShouldReturnInstanceRule`: which class-side methods with the pragma `<sampleInstance>` are reported for not returning an instance of their class.

## Responsibility:

- Check that a method returning nothing, the class or another object is reported.
- Check that a method returning an instance, in one of its returns or through the class named directly, is not reported, nor an instance-side method or a method without the pragma.

## Collaborators:

- `ReAbstractSampleInstanceRuleTest`: compiles the methods and answers the critiques.

```
self assert: (self critiquesOfClassMethod: 'foo <sampleInstance> ^ 42') size equals: 1
```
"
Class {
	#name : 'ReSampleInstanceShouldReturnInstanceRuleTest',
	#superclass : 'ReAbstractSampleInstanceRuleTest',
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testInstanceSideSampleInstanceIsNotReported [

	self assert: (self critiquesOfInstanceMethod: 'foo <sampleInstance> ^ 42') size equals: 0
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testMethodWithoutSampleInstanceIsNotReported [

	self assert: (self critiquesOfClassMethod: 'exampleFoo ^ 42') size equals: 0
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testSampleInstanceNamingItsClassIsNotReported [

	self assert: (self critiquesOfClassMethodNamed: #foo source: [ :class | 'foo <sampleInstance> | foo | foo := {1} new. ^ foo' format: { class name } ]) size equals: 0
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testSampleInstanceReturningInstanceInOneBranchIsNotReported [

	self assert: (self critiquesOfClassMethod: 'foo <sampleInstance> 1 > 2 ifTrue: [ ^ nil ]. ^ self new') size equals: 0
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testSampleInstanceReturningNewIsNotReported [

	self assert: (self critiquesOfClassMethod: 'foo <sampleInstance> ^ self new') size equals: 0
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testSampleInstanceReturningOtherObjectIsReported [

	self assert: (self critiquesOfClassMethod: 'foo <sampleInstance> ^ 42') size equals: 1
]

{ #category : 'tests' }
ReSampleInstanceShouldReturnInstanceRuleTest >> testSampleInstanceReturningSelfIsReported [

	self assert: (self critiquesOfClassMethod: 'foo <sampleInstance> self new printString') size equals: 1
]
