"
I am the abstract smoke test of the class-side `<sampleInstance>` methods: each must answer an instance of its class.

Like every subclass of `SmokePragmaMethodsTest`, I check nothing by myself: each project subclasses me in its own test package and overrides the class-side hook `packageNamesToCheck` to name its packages. I complement the static rule `ReSampleInstanceShouldReturnInstanceRule` by running the methods.

```
SmokeSampleInstanceMethodsTest << #MyProjectSampleInstanceMethodsTest
	package: 'MyProject-Tests'.

MyProjectSampleInstanceMethodsTest class >> packageNamesToCheck
	^ #( 'MyProject-Core' )
```

## Responsibility:

- Select the methods with `<sampleInstance>` (`pragmaSelector`).
- Run each method and check that its result is an instance of the class defining it, or of a subclass (`testSampleInstanceAnswersAnInstanceOfItsClass`).

## Collaborators:

- `SmokePragmaMethodsTest`: my superclass; it collects the methods, checks that there is something to check and runs them.

## Public API and Key Messages

- In a project subclass, override `packageNamesToCheck`, and optionally `excludedMethods`.
- `MyProjectSampleInstanceMethodsTest suite run` runs one test per method.
"
Class {
	#name : 'SmokeSampleInstanceMethodsTest',
	#superclass : 'SmokePragmaMethodsTest',
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'private' }
SmokeSampleInstanceMethodsTest class >> isAbstract [

	^ self == SmokeSampleInstanceMethodsTest
]

{ #category : 'accessing' }
SmokeSampleInstanceMethodsTest class >> pragmaSelector [

	^ #sampleInstance
]

{ #category : 'tests' }
SmokeSampleInstanceMethodsTest >> testSampleInstanceAnswersAnInstanceOfItsClass [

	self assert: (self runMethod class includesBehavior: method methodClass instanceSide)
]
