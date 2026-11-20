"
I am the abstract superclass of the tests of the rules about sample instances. I compile a method in a new class and answer the critiques of the rule under test on it.

## Responsibility:

- Create the classes of a test with a `ClassFactoryForTestCase`, removed in `tearDown`.
- Answer the critiques on a class-side or an instance-side method.

## Collaborators:

- `ReExampleShouldBeSampleInstanceRuleTest` and `ReSampleInstanceShouldReturnInstanceRuleTest`: my subclasses.
- `ReAbstractRuleTestCase`: runs the rule under test (`myCritiquesOnMethod:`).

## Public API and Key Messages

- `critiquesOfClassMethod: aSource` and `critiquesOfClassMethod: aSource with: someProtocolsAndSources`.
- `critiquesOfClassMethodNamed: aSelector source: aBlock`, whose block gets the new class, to name it in the source.
- `critiquesOfInstanceMethod: aSource`.

```
self assert: (self critiquesOfClassMethod: 'exampleFoo ^ self new') size equals: 1
```

## Internal Representation

- `classFactory`: creates the classes in an empty environment, so a source can name only these classes.
"
Class {
	#name : 'ReAbstractSampleInstanceRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'testing' }
ReAbstractSampleInstanceRuleTest class >> isAbstract [

	^ self == ReAbstractSampleInstanceRuleTest
]

{ #category : 'helpers' }
ReAbstractSampleInstanceRuleTest >> critiquesOfClassMethod: aSource [
	"Answer the critiques of my rule on aSource, compiled on the class side of a new class."

	^ self critiquesOfClassMethod: aSource with: #(  )
]

{ #category : 'helpers' }
ReAbstractSampleInstanceRuleTest >> critiquesOfClassMethod: aSource with: someProtocolsAndSources [
	"Answer the critiques of my rule on aSource, compiled on the class side of a new class having also the class methods of someProtocolsAndSources (protocol -> source)."

	| class selector |
	class := classFactory make: [ :aBuilder | aBuilder ].
	someProtocolsAndSources do: [ :each | class class compile: each value classified: each key ].
	selector := class class compile: aSource classified: #examples.
	^ self myCritiquesOnMethod: class class >> selector
]

{ #category : 'helpers' }
ReAbstractSampleInstanceRuleTest >> critiquesOfClassMethodNamed: aSelector source: aBlock [
	"Answer the critiques of my rule on the class method answered by aBlock for a new class."

	| class |
	class := classFactory make: [ :aBuilder | aBuilder ].
	class class compile: (aBlock value: class) classified: #examples.
	^ self myCritiquesOnMethod: class class >> aSelector
]

{ #category : 'helpers' }
ReAbstractSampleInstanceRuleTest >> critiquesOfInstanceMethod: aSource [
	"Answer the critiques of my rule on aSource, compiled on the instance side of a new class."

	| class selector |
	class := classFactory make: [ :aBuilder | aBuilder ].
	selector := class compile: aSource classified: #examples.
	^ self myCritiquesOnMethod: class >> selector
]

{ #category : 'running' }
ReAbstractSampleInstanceRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new
]

{ #category : 'running' }
ReAbstractSampleInstanceRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]
