"
I verify `SmokeSampleInstanceMethodsReturnInstanceTest`: every class-side `<sampleInstance>` method of the packages to check must answer an instance of its class.

## Responsibility:

- Fill the fixture class with sample instances answering an instance of their class, of another class, or breaking.
- Check that a project subclass runs the three of them and fails on the two wrong ones.
- Inherit from `SmokeVerifierTest` the checks common to every smoke test.

## Collaborators:

- `SmokeVerifierTest`: my superclass; it makes the fixture class and holds the checks common to every smoke test.
- `SmokeSampleInstanceMethodsReturnInstanceTest`: the smoke test I subclass and run.

## Public API and Key Messages

- `smokeTestClass`, `compileFixtureMethods`, `fixtureMethodCount`, `fixtureMethodToExclude`: the hooks of `SmokeVerifierTest`.
- Run my tests:

```
SmokeSampleInstanceVerifierTest suite run
```
"
Class {
	#name : 'SmokeSampleInstanceVerifierTest',
	#superclass : 'SmokeVerifierTest',
	#category : 'SUnit-SmokeVerifier-Tests',
	#package : 'SUnit-SmokeVerifier-Tests'
}

{ #category : 'hooks' }
SmokeSampleInstanceVerifierTest >> compileFixtureMethods [
	"Compile sample instances answering an instance of their class, of another class, or breaking."

	fixtureClass class
		compile: 'sampleInstanceOfItsClass
	<sampleInstance>
	^ self new'
		classified: #fixture.
	fixtureClass class
		compile: 'sampleInstanceOfAnotherClass
	<sampleInstance>
	^ 42'
		classified: #fixture.
	fixtureClass class
		compile: 'breakingSampleInstance
	<sampleInstance>
	1 / 0.
	^ self new'
		classified: #fixture
]

{ #category : 'hooks' }
SmokeSampleInstanceVerifierTest >> fixtureMethodToExclude [

	^ 'SmokeVerifierFixtureClass class>>#breakingSampleInstance'
]

{ #category : 'hooks' }
SmokeSampleInstanceVerifierTest >> smokeTestClass [

	^ SmokeSampleInstanceMethodsReturnInstanceTest
]

{ #category : 'tests - running' }
SmokeSampleInstanceVerifierTest >> testSampleInstanceTestChecksTheClassOfEveryInstance [

	| result |
	result := (self smokeTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 3.
	self assert: (self failingMethodsIn: result) size equals: 2.
	self assert: ((self failingMethodsIn: result) includes: #sampleInstanceOfAnotherClass).
	self assert: ((self failingMethodsIn: result) includes: #breakingSampleInstance)
]
