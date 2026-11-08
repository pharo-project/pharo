"
I am the smoke test running every class-side method with the pragma `<sampleInstance>` of the packages to check: a test fails when its method does not answer an instance of its class.

## Responsibility:

- Answer `#sampleInstance` as the pragma of the methods to run.
- Check that the object answered by each method is a kind of its class.

## Collaborators:

- `TSmokePragmaMethodsTest`: finds the methods, runs them and fails when nothing is checked.
- `ProjectSmokeVerifierGenerator`: generates the class of a project using me (`generateSampleInstanceTest`).
- `ReSampleInstanceShouldReturnInstanceRule`: checks the same property statically.

## Public API and Key Messages

- `testSampleInstanceAnswersAnInstanceOfItsClass` runs the method of the test and checks the class of its result.

```
(ProjectSmokeVerifierGenerator new
	projectName: 'MyProject';
	packageNames: #( 'MyProject' );
	testPackageName: 'MyProject-Tests';
	generateSampleInstanceTest) suite run
```
"
Trait {
	#name : 'TSmokeSampleInstanceMethodsTest',
	#traits : 'TSmokePragmaMethodsTest',
	#classTraits : 'TSmokePragmaMethodsTest classTrait',
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'accessing' }
TSmokeSampleInstanceMethodsTest classSide >> pragmaSelector [

	^ #sampleInstance
]

{ #category : 'tests' }
TSmokeSampleInstanceMethodsTest >> testSampleInstanceAnswersAnInstanceOfItsClass [

	self assert: (self runMethod class includesBehavior: method methodClass instanceSide)
]
