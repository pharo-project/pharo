"
I test `ProjectSmokeVerifierGenerator` and, through the classes it generates, the traits `TSmokeScriptMethodsTest` and `TSmokeSampleInstanceMethodsTest`.

## Responsibility:

- Check the generated classes: names, superclass, traits, packages checked, excluded methods kept on regeneration.
- Check that they run every method with the pragma, report the broken ones, skip the excluded ones and close the windows opened.
- Check that they fail when they check nothing: missing package, no package, no method.

## Collaborators:

- `ShiftClassInstaller`: creates the fixture class `SmokeVerifierFixtureClass` in the package `SUnit-SmokeVerifier-Tests-Fixture`, removed with the generated classes in `tearDown`.
- `ProjectSmokeVerifierGenerator`: generates the classes of the fixture project `SmokeVerifierFixture` (`generatorFor:`).

## Public API and Key Messages

- `scriptTestFor: someNames` and `sampleInstanceTestFor: someNames` answer a generated class checking the packages named someNames.
- `failingMethodsIn: aTestResult` answers the selectors of the fixture methods whose test failed.

```
self assert: (self failingMethodsIn: (self scriptTestFor: { self fixturePackageName }) suite run) equals: #( #scriptBreaking )
```

## Internal Representation

- `fixtureClass`: has two `<script>` and two `<sampleInstance>` class-side methods, one right and one broken each, and an instance-side `<script>` method that must not be run.
"
Class {
	#name : 'ProjectSmokeVerifierGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'fixtureClass'
	],
	#category : 'SUnit-SmokeVerifier-Tests',
	#package : 'SUnit-SmokeVerifier-Tests'
}

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> emptyPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-Empty'
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> failingMethodsIn: aTestResult [
	"Answer the selectors of the methods whose test fails or errs in aTestResult. SUnit clears the slots of a test after running it, so the method is taken from the parameters of the test."

	^ (aTestResult failures , aTestResult errors) asArray collect: [ :each | each parametersToUse first valuable selector ]
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> failureMessageIn: aTestResult [
	"Answer the message of the first failure of aTestResult, by running its test again."

	^ [ aTestResult failures anyOne debug. nil ] on: TestFailure do: [ :failure | failure messageText ]
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> fixturePackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture'
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> generatorFor: someNames [
	"Answer a generator for the fixture project, checking the packages named someNames and installing its classes in the fixture package."

	^ ProjectSmokeVerifierGenerator new
		projectName: 'SmokeVerifierFixture';
		packageNames: someNames;
		testPackageName: self fixturePackageName;
		yourself
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> missingPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-NotLoaded'
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> sampleInstanceTestFor: someNames [

	^ (self generatorFor: someNames) generateSampleInstanceTest
]

{ #category : 'helpers' }
ProjectSmokeVerifierGeneratorTest >> scriptTestFor: someNames [

	^ (self generatorFor: someNames) generateScriptTest
]

{ #category : 'running' }
ProjectSmokeVerifierGeneratorTest >> setUp [

	super setUp.
	fixtureClass := ShiftClassInstaller make: [ :builder |
		builder
			name: #SmokeVerifierFixtureClass;
			superclass: Object;
			package: self fixturePackageName ].
	fixtureClass compile: 'instanceSideScript
	<script>
	^ 1 / 0' classified: #fixture.
	fixtureClass class compile: 'scriptRunning
	<script>
	^ 3 + 4' classified: #fixture.
	fixtureClass class compile: 'scriptBreaking
	<script>
	^ 1 / 0' classified: #fixture.
	fixtureClass class compile: 'sampleInstanceOfItsClass
	<sampleInstance>
	^ self new' classified: #fixture.
	fixtureClass class compile: 'sampleInstanceOfAnotherClass
	<sampleInstance>
	^ 42' classified: #fixture
]

{ #category : 'running' }
ProjectSmokeVerifierGeneratorTest >> tearDown [

	{ self fixturePackageName. self emptyPackageName } do: [ :name |
		(PackageOrganizer default packageNamed: name ifAbsent: [ nil ]) ifNotNil: [ :package |
			package definedClasses do: [ :each | each removeFromSystem ].
			package removeFromSystem ] ].
	super tearDown
]

{ #category : 'tests - running' }
ProjectSmokeVerifierGeneratorTest >> testExcludedMethodIsNotRun [

	| script |
	script := self scriptTestFor: { self fixturePackageName }.
	script class compile: 'excludedMethods
	^ #( ''SmokeVerifierFixtureClass class>>#scriptBreaking'' )' classified: #accessing.
	self assert: script suite run runCount equals: 1
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testFailureNamesTheMissingPackage [

	| result |
	result := (self scriptTestFor: { self missingPackageName }) suite run.
	self assert: (self failureMessageIn: result) equals: ('Packages to check not loaded: {1}' format: { self missingPackageName })
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testFailureSaysThereIsNoPackage [

	| result |
	result := (self scriptTestFor: #(  )) suite run.
	self assert: (self failureMessageIn: result) equals: 'No package to check'
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testGenerateAnswersBothClasses [

	self
		assert: ((self generatorFor: { self fixturePackageName }) generate collect: [ :each | each name ])
		equals: #( #SmokeVerifierFixtureScriptMethodsTest #SmokeVerifierFixtureSampleInstanceMethodsTest )
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testGeneratedClassChecksTheGivenPackages [

	self assert: (self scriptTestFor: { self fixturePackageName }) packageNamesToCheck equals: { self fixturePackageName }
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testGeneratedClassDefinesOnlyItsPackagesAndExclusions [

	| script |
	script := self scriptTestFor: { self fixturePackageName }.
	self assert: script localSelectors isEmpty.
	self assert: script class localSelectors asSet equals: #( #packageNamesToCheck #excludedMethods ) asSet
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testGeneratedClassIsConcrete [

	self deny: (self scriptTestFor: { self fixturePackageName }) isAbstract
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testGeneratedClassesUseTheTraits [

	| script sampleInstance |
	script := self scriptTestFor: { self fixturePackageName }.
	sampleInstance := self sampleInstanceTestFor: { self fixturePackageName }.
	self assert: script superclass identicalTo: ParametrizedTestCase.
	self assert: (script traitComposition includesTrait: TSmokeScriptMethodsTest).
	self assert: sampleInstance superclass identicalTo: ParametrizedTestCase.
	self assert: (sampleInstance traitComposition includesTrait: TSmokeSampleInstanceMethodsTest).
	self assert: script package name equals: self fixturePackageName
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testMissingPackageAmongOthersFailsEveryTest [

	| result |
	result := (self scriptTestFor: { self fixturePackageName. self missingPackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: result failures size equals: 2
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testMissingPackageFails [

	| result |
	result := (self scriptTestFor: { self missingPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testNoPackageFails [

	| result |
	result := (self sampleInstanceTestFor: #(  )) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
ProjectSmokeVerifierGeneratorTest >> testPackageWithoutMethodFails [

	| result |
	PackageOrganizer default ensurePackage: self emptyPackageName.
	result := (self scriptTestFor: { self emptyPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - generation' }
ProjectSmokeVerifierGeneratorTest >> testRegenerationKeepsTheExcludedMethods [

	| script |
	script := self scriptTestFor: { self fixturePackageName }.
	script class compile: 'excludedMethods
	^ #( ''SmokeVerifierFixtureClass class>>#scriptBreaking'' )' classified: #accessing.
	self scriptTestFor: { self fixturePackageName }.
	self assert: script excludedMethods equals: #( 'SmokeVerifierFixtureClass class>>#scriptBreaking' )
]

{ #category : 'tests - running' }
ProjectSmokeVerifierGeneratorTest >> testSampleInstanceTestChecksTheClassOfEveryInstance [

	| result |
	result := (self sampleInstanceTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #sampleInstanceOfAnotherClass )
]

{ #category : 'tests - running' }
ProjectSmokeVerifierGeneratorTest >> testScriptTestRunsEveryScript [

	| result |
	result := (self scriptTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #scriptBreaking )
]

{ #category : 'tests - running' }
ProjectSmokeVerifierGeneratorTest >> testWindowOpenedByScriptIsClosed [

	| world before |
	world := Smalltalk globals at: #World ifAbsent: [ ^ self skip ].
	fixtureClass class compile: 'scriptOpeningWindow
	<script>
	^ SystemWindow new openInWorld' classified: #fixture.
	before := world submorphs size.
	self assert: (self scriptTestFor: { self fixturePackageName }) suite run runCount equals: 3.
	self assert: world submorphs size equals: before
]
