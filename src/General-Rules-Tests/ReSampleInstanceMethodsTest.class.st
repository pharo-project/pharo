"
I run every class-side method with the pragma `<sampleInstance>` of the packages to check, and fail when it does not answer an instance of its class.

## Responsibility:

- Answer `#sampleInstance` as the pragma of the methods to run.
- Check that the object answered by each method is a kind of its class.

## Collaborators:

- `RePragmaMethodsTest`: finds the methods, runs them and makes copies of me.
- `ReSampleInstanceShouldReturnInstanceRule`: checks the same property statically.

## Public API and Key Messages

- `testSampleInstanceAnswersAnInstanceOfItsClass` runs the method of the test and checks the class of its result.
- Use me through a copy for a project:

```
ReSampleInstanceMethodsTest copyNamed: #MyProjectSampleInstanceMethodsTest forPackagesNamed: #( 'MyProject' ) in: 'MyProject-Tests'
```
"
Class {
	#name : 'ReSampleInstanceMethodsTest',
	#superclass : 'RePragmaMethodsTest',
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'private' }
ReSampleInstanceMethodsTest class >> isAbstract [

	^ self == ReSampleInstanceMethodsTest
]

{ #category : 'accessing' }
ReSampleInstanceMethodsTest class >> pragmaSelector [

	^ #sampleInstance
]

{ #category : 'tests' }
ReSampleInstanceMethodsTest >> testSampleInstanceAnswersAnInstanceOfItsClass [

	self assert: (self runMethod isKindOf: method methodClass instanceSide)
]
