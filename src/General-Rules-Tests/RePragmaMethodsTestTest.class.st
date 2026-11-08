"
I check `ReScriptMethodsTest` and `ReSampleInstanceMethodsTest` through copies of them checking a fixture package.

## Responsibility:

- Check that a copy runs every method with the pragma, reports the broken ones and skips the excluded ones.
- Check that a copy is concrete, checks the given packages and depends only on `SUnit-Core`.
- Check that the windows opened by a script are closed.

## Collaborators:

- `ShiftClassInstaller`: creates the fixture class `RePragmaMethodsTestFixture` in the package `General-Rules-Tests-Fixture`, removed with the copies in `tearDown`.
- `RePragmaMethodsTest class>>#copyNamed:forPackagesNamed:in:`: makes the copies.

## Public API and Key Messages

- `resultOfCopyOf: aTestClass` answers the result of a copy of aTestClass run on the fixture package.
- `failingMethodsIn: aTestResult` answers the selectors of the fixture methods whose test failed. For example:

```
self assert: (self failingMethodsIn: (self resultOfCopyOf: ReScriptMethodsTest)) equals: #( #scriptBreaking )
```

## Internal Representation

- `fixtureClass`: has two `<script>` and two `<sampleInstance>` class-side methods, one right and one broken each, and an instance-side `<script>` method that must not be run.
"
Class {
	#name : 'RePragmaMethodsTestTest',
	#superclass : 'TestCase',
	#instVars : [
		'fixtureClass'
	],
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'helpers' }
RePragmaMethodsTestTest >> copyOf: aTestClass [
	"Answer a copy of aTestClass checking the fixture package, installed in the fixture package."

	^ aTestClass copyNamed: (#RePragmaMethodsTestFixtureCopyOf , aTestClass name) asSymbol forPackagesNamed: { self fixturePackageName } in: self fixturePackageName
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> copyOf: aTestClass forPackagesNamed: someNames [
	"Answer a copy of aTestClass checking the packages named someNames, installed in the fixture package."

	^ aTestClass copyNamed: (#RePragmaMethodsTestFixtureCopyOf , aTestClass name) asSymbol forPackagesNamed: someNames in: self fixturePackageName
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> emptyPackageName [

	^ 'General-Rules-Tests-Fixture-Empty'
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> failingMethodsIn: aTestResult [
	"Answer the selectors of the methods whose test fails or errs in aTestResult. SUnit clears the slots of a test after running it, so the method is taken from the parameters of the test."

	^ (aTestResult failures , aTestResult errors) asArray collect: [ :each | each parametersToUse first valuable selector ]
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> failureMessageIn: aTestResult [
	"Answer the message of the first failure of aTestResult, by running its test again."

	^ [ aTestResult failures anyOne debug. nil ] on: TestFailure do: [ :failure | failure messageText ]
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> fixturePackageName [

	^ 'General-Rules-Tests-Fixture'
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> missingPackageName [

	^ 'General-Rules-Tests-Fixture-NotLoaded'
]

{ #category : 'helpers' }
RePragmaMethodsTestTest >> resultOfCopyOf: aTestClass [
	"Answer the result of running the suite of a copy of aTestClass on the fixture package."

	^ (self copyOf: aTestClass) suite run
]

{ #category : 'running' }
RePragmaMethodsTestTest >> setUp [

	super setUp.
	fixtureClass := ShiftClassInstaller make: [ :builder |
		builder
			name: #RePragmaMethodsTestFixture;
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
RePragmaMethodsTestTest >> tearDown [

	{ self fixturePackageName. self emptyPackageName } do: [ :name |
		(PackageOrganizer default packageNamed: name ifAbsent: [ nil ]) ifNotNil: [ :package |
			package definedClasses do: [ :each | each removeFromSystem ].
			package removeFromSystem ] ].
	super tearDown
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyCheckingAMissingPackageAmongOthersFailsEveryTest [

	| result |
	result := (self copyOf: ReScriptMethodsTest forPackagesNamed: { self fixturePackageName. self missingPackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: result failures size equals: 2
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyCheckingAMissingPackageFails [

	| result |
	result := (self copyOf: ReScriptMethodsTest forPackagesNamed: { self missingPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyCheckingAPackageWithoutMethodFails [

	| result |
	PackageOrganizer default ensurePackage: self emptyPackageName.
	result := (self copyOf: ReScriptMethodsTest forPackagesNamed: { self emptyPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyCheckingNoPackageFails [

	| result |
	result := (self copyOf: ReSampleInstanceMethodsTest forPackagesNamed: #(  )) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyChecksTheGivenPackages [

	self assert: (self copyOf: ReScriptMethodsTest) packageNamesToCheck equals: { self fixturePackageName }
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyDependsOnlyOnSUnit [

	| copy |
	copy := self copyOf: ReScriptMethodsTest.
	self assert: copy superclass identicalTo: ParametrizedTestCase.
	(copy methods , copy class methods) do: [ :each |
		self deny: (each literals anySatisfy: [ :literal |
			literal isVariableBinding and: [ literal value isClass and: [ literal value package name = 'General-Rules-Tests' ] ] ]) ]
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testCopyIsConcrete [

	self deny: (self copyOf: ReScriptMethodsTest) isAbstract
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testExcludedMethodIsNotRun [

	| copy |
	copy := self copyOf: ReScriptMethodsTest.
	copy class compile: 'excludedMethods
	^ #( ''RePragmaMethodsTestFixture class>>#scriptBreaking'' )' classified: #accessing.
	self assert: copy suite run runCount equals: 1
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testFailureNamesTheMissingPackage [

	| result |
	result := (self copyOf: ReScriptMethodsTest forPackagesNamed: { self missingPackageName }) suite run.
	self assert: (self failureMessageIn: result) equals: ('Packages to check not loaded: {1}' format: { self missingPackageName })
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testFailureSaysThereIsNoPackage [

	| result |
	result := (self copyOf: ReScriptMethodsTest forPackagesNamed: #(  )) suite run.
	self assert: (self failureMessageIn: result) equals: 'No package to check'
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testOriginalsAreAbstract [

	self assert: RePragmaMethodsTest isAbstract.
	self assert: ReScriptMethodsTest isAbstract.
	self assert: ReSampleInstanceMethodsTest isAbstract
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testSampleInstanceCopyChecksTheClassOfEveryInstance [

	| result |
	result := self resultOfCopyOf: ReSampleInstanceMethodsTest.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #sampleInstanceOfAnotherClass )
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testScriptCopyRunsEveryScript [

	| result |
	result := self resultOfCopyOf: ReScriptMethodsTest.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #scriptBreaking )
]

{ #category : 'tests' }
RePragmaMethodsTestTest >> testWindowOpenedByScriptIsClosed [

	| world before |
	world := Smalltalk globals at: #World ifAbsent: [ ^ self skip ].
	fixtureClass class compile: 'scriptOpeningWindow
	<script>
	^ SystemWindow new openInWorld' classified: #fixture.
	before := world submorphs size.
	self assert: (self resultOfCopyOf: ReScriptMethodsTest) runCount equals: 3.
	self assert: world submorphs size equals: before
]
