"
I am the abstract test of the smoke tests of `SUnit-SmokeVerifier`: I check them the way a project uses them, by subclassing one and overriding `packageNamesToCheck`. Each of my subclasses verifies one smoke test, so the script concern and the sample instance concern never mix.

## Responsibility:

- Make a fixture class in `setUp` and remove it, and the project subclasses, in `tearDown`; my subclass fills it with its own methods.
- Check what every smoke test must do: run every method of the packages to check and skip the excluded ones.
- Check that a smoke test never passes while checking nothing: hook not overridden, package misspelled, not loaded or without such a method.

## Collaborators:

- `SmokeScriptVerifierTest`: my subclass verifying `SmokeScriptMethodsShouldNotBreakTest`.
- `SmokeSampleInstanceVerifierTest`: my subclass verifying `SmokeSampleInstanceMethodsReturnInstanceTest`.
- `SmokePragmaMethodsTest` subclasses: I define project subclasses of them in the fixture package and run their suites.

## Public API and Key Messages

Hooks for my subclasses:
- `smokeTestClass`: the smoke test verified.
- `compileFixtureMethods`: compile in `fixtureClass` the methods that this smoke test runs or ignores.
- `fixtureMethodToExclude`: one of the class-side methods, to exclude. The smoke test must run every class-side method of `fixtureClass`.

Helpers:
- `smokeTestFor:` defines a project subclass of `smokeTestClass` checking the given packages; `smokeTestNotOverridingTheHook` one that does not override the hook.

Run `SmokeVerifierTest class >> #scriptRunEveryVerifier` to run both verifiers:

```
SmokeScriptVerifierTest suite run.
SmokeSampleInstanceVerifierTest suite run.
```

## Internal Representation

- `fixtureClass`: the fixture class, `SmokeVerifierFixtureClass`.

### Implementation Points

- The fixture class and the project subclasses live in the package `SUnit-SmokeVerifier-Tests-Fixture`; `tearDown` removes it, and the empty package `SUnit-SmokeVerifier-Tests-Fixture-Empty`.
- I am abstract (`isAbstract`): my tests run once per subclass.
- SUnit clears the slots of a test after running it, so `failingMethodsIn:` reads the method of a failed test from its parameters.
"
Class {
	#name : 'SmokeVerifierTest',
	#superclass : 'TestCase',
	#instVars : [
		'fixtureClass'
	],
	#category : 'SUnit-SmokeVerifier-Tests',
	#package : 'SUnit-SmokeVerifier-Tests'
}

{ #category : 'testing' }
SmokeVerifierTest class >> isAbstract [

	^ self == SmokeVerifierTest
]

{ #category : 'script' }
SmokeVerifierTest class >> scriptRunEveryVerifier [
	"Run the verifier of each smoke test of SUnit-SmokeVerifier and inspect their results."

	<example>
	(self allSubclasses select: [ :each | each isAbstract not ] thenCollect: [ :each | each suite run ]) inspect
]

{ #category : 'hooks' }
SmokeVerifierTest >> compileFixtureMethods [
	"Compile in the fixture class the methods that the smoke test I verify runs or ignores."

	self subclassResponsibility
]

{ #category : 'helpers' }
SmokeVerifierTest >> emptyPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-Empty'
]

{ #category : 'helpers' }
SmokeVerifierTest >> failingMethodsIn: aTestResult [
	"Answer the selectors of the methods whose test fails or errs in aTestResult. 
	SUnit clears the slots of a test after running it, so the method is taken from the parameters of the test."

	^ (aTestResult failures , aTestResult errors) asArray collect: [ :each | each parametersToUse first valuable selector ]
]

{ #category : 'helpers' }
SmokeVerifierTest >> failureMessageIn: aTestResult [
	"Answer the message of the first failure of aTestResult, by running its test again."

	^ [ aTestResult failures anyOne debug. nil ] on: TestFailure do: [ :failure | failure messageText ]
]

{ #category : 'hooks' }
SmokeVerifierTest >> fixtureMethodToExclude [
	"Answer a fixture method run by the smoke test I verify, as a string such as 'SmokeVerifierFixtureClass class>>#scriptBreaking', to check that excluding it skips it."

	^ self subclassResponsibility
]

{ #category : 'helpers' }
SmokeVerifierTest >> fixturePackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture'
]

{ #category : 'helpers' }
SmokeVerifierTest >> missingPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-NotLoaded'
]

{ #category : 'running' }
SmokeVerifierTest >> setUp [

	super setUp.
	fixtureClass := ShiftClassInstaller make: [ :builder |
			                builder
				                name: #SmokeVerifierFixtureClass;
				                superclass: Object;
				                package: self fixturePackageName ].
	self compileFixtureMethods
]

{ #category : 'hooks' }
SmokeVerifierTest >> smokeTestClass [
	"Answer the abstract smoke test of SUnit-SmokeVerifier that I verify."

	^ self subclassResponsibility
]

{ #category : 'helpers' }
SmokeVerifierTest >> smokeTestFor: someNames [
	"Answer a project subclass of the smoke test I verify, checking the packages named someNames."

	^ self subclassOf: self smokeTestClass named: #SmokeVerifierFixtureProjectTest checking: someNames
]

{ #category : 'helpers' }
SmokeVerifierTest >> smokeTestNotOverridingTheHook [
	"Answer a project subclass of the smoke test I verify that does not override packageNamesToCheck."

	^ self subclassOf: self smokeTestClass named: #SmokeVerifierFixtureProjectTest
]

{ #category : 'helpers' }
SmokeVerifierTest >> subclassOf: aClass named: aSymbol [
	"Answer a subclass of aClass in the fixture package, as a project defines it, without overriding packageNamesToCheck."

	^ aClass << aSymbol
		package: self fixturePackageName;
		install
]

{ #category : 'helpers' }
SmokeVerifierTest >> subclassOf: aClass named: aSymbol checking: someNames [
	"Answer a subclass of aClass in the fixture package whose packageNamesToCheck answers someNames, as a project defines it."

	| class |
	class := self subclassOf: aClass named: aSymbol.
	class class compile: ('packageNamesToCheck{1}{1}{2}^ {3}' format: { String cr. String tab. someNames asArray printString }) classified: #accessing.
	^ class
]

{ #category : 'running' }
SmokeVerifierTest >> tearDown [

	{ self fixturePackageName. self emptyPackageName } do: [ :name |
		(PackageOrganizer default packageNamed: name ifAbsent: [ nil ]) ifNotNil: [ :package |
			package definedClasses do: [ :each | each removeFromSystem ].
			package removeFromSystem ] ].
	super tearDown
]

{ #category : 'tests - running' }
SmokeVerifierTest >> testEveryFixtureMethodIsRun [

	self assert: (self smokeTestFor: { self fixturePackageName }) suite run runCount equals: fixtureClass class selectors size
]

{ #category : 'tests - running' }
SmokeVerifierTest >> testExcludedMethodIsNotRun [

	| smokeTest |
	smokeTest := self smokeTestFor: { self fixturePackageName }.
	smokeTest class
		compile: ('excludedMethods
	^ {1}' format: { { self fixtureMethodToExclude } printString })
		classified: #accessing.
	self assert: smokeTest suite run runCount equals: fixtureClass class selectors size - 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testFailureNamesTheMissingPackage [

	| result |
	result := (self smokeTestFor: { self missingPackageName }) suite run.
	self
		assert: (self failureMessageIn: result)
		equals: ('Packages to check not found, misspelled or not loaded: {1}' format: { self missingPackageName })
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testFailureSaysThereIsNoMethod [

	| result |
	PackageOrganizer default ensurePackage: self emptyPackageName.
	result := (self smokeTestFor: { self emptyPackageName }) suite run.
	self assert: (self failureMessageIn: result) equals: 'No method to check in the packages to check'
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testFailureSaysThereIsNoPackage [

	| result |
	result := self smokeTestNotOverridingTheHook suite run.
	self assert: (self failureMessageIn: result) equals: 'No package to check'
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMissingPackageAmongOthersFailsEveryTest [

	| result |
	result := (self smokeTestFor: {
			           self fixturePackageName.
			           self missingPackageName }) suite run.
	self assert: result runCount equals: fixtureClass class selectors size.
	self assert: result failures size equals: fixtureClass class selectors size
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMissingPackageFails [

	| result |
	result := (self smokeTestFor: { self missingPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMisspelledPackageFails [

	| result |
	result := (self smokeTestFor: { 'SUnit-SmokeVerfier-Tests-Fixture' }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testPackageWithoutMethodFails [

	| result |
	PackageOrganizer default ensurePackage: self emptyPackageName.
	result := (self smokeTestFor: { self emptyPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testSubclassNotOverridingTheHookFails [

	| result |
	result := self smokeTestNotOverridingTheHook suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]
