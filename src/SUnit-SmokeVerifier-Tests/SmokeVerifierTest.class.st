"
I test the smoke tests of `SUnit-SmokeVerifier` the way a project uses them: by subclassing `SmokeScriptMethodsTest` and `SmokeSampleInstanceMethodsTest` and overriding `packageNamesToCheck`.

## Responsibility:

- Check that the three smoke test classes are abstract and that a project subclass is concrete.
- Check that a project subclass runs every method with the pragma, reports the broken ones and skips the excluded ones.
- Check that a subclass never passes while checking nothing: hook not overridden, package misspelled or not loaded, package without such a method.

## Collaborators:

- `SmokeScriptMethodsTest`, `SmokeSampleInstanceMethodsTest`: subclassed by my helpers `scriptTestFor:` and `sampleInstanceTestFor:`; I run the suites of these subclasses.
- `SmokeVerifierFixtureClass`: a fixture class made in `setUp`, with scripts and sample instances that work or break.

## Public API and Key Messages

- `subclassOf:named:checking:` defines a project subclass checking the given packages; `subclassOf:named:` one that does not override the hook.

## Internal Representation

- `fixtureClass`: the fixture class.

### Implementation Points

- The fixture class and the project subclasses live in the package `SUnit-SmokeVerifier-Tests-Fixture`; `tearDown` removes it, and the empty package `SUnit-SmokeVerifier-Tests-Fixture-Empty`.
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

{ #category : 'helpers' }
SmokeVerifierTest >> emptyPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-Empty'
]

{ #category : 'helpers' }
SmokeVerifierTest >> failingMethodsIn: aTestResult [
	"Answer the selectors of the methods whose test fails or errs in aTestResult. SUnit clears the slots of a test after running it, so the method is taken from the parameters of the test."

	^ (aTestResult failures , aTestResult errors) asArray collect: [ :each | each parametersToUse first valuable selector ]
]

{ #category : 'helpers' }
SmokeVerifierTest >> failureMessageIn: aTestResult [
	"Answer the message of the first failure of aTestResult, by running its test again."

	^ [ aTestResult failures anyOne debug. nil ] on: TestFailure do: [ :failure | failure messageText ]
]

{ #category : 'helpers' }
SmokeVerifierTest >> fixturePackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture'
]

{ #category : 'helpers' }
SmokeVerifierTest >> missingPackageName [

	^ 'SUnit-SmokeVerifier-Tests-Fixture-NotLoaded'
]

{ #category : 'helpers' }
SmokeVerifierTest >> sampleInstanceTestFor: someNames [

	^ self subclassOf: SmokeSampleInstanceMethodsTest named: #SmokeVerifierFixtureSampleInstanceMethodsTest checking: someNames
]

{ #category : 'helpers' }
SmokeVerifierTest >> scriptTestFor: someNames [

	^ self subclassOf: SmokeScriptMethodsTest named: #SmokeVerifierFixtureScriptMethodsTest checking: someNames
]

{ #category : 'running' }
SmokeVerifierTest >> setUp [

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
SmokeVerifierTest >> testExcludedMethodIsNotRun [

	| script |
	script := self scriptTestFor: { self fixturePackageName }.
	script class compile: 'excludedMethods
	^ #( ''SmokeVerifierFixtureClass class>>#scriptBreaking'' )' classified: #accessing.
	self assert: script suite run runCount equals: 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testFailureNamesTheMissingPackage [

	| result |
	result := (self scriptTestFor: { self missingPackageName }) suite run.
	self assert: (self failureMessageIn: result) equals: ('Packages to check not found, misspelled or not loaded: {1}' format: { self missingPackageName })
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testFailureSaysThereIsNoPackage [

	| result |
	result := (self subclassOf: SmokeScriptMethodsTest named: #SmokeVerifierFixtureScriptMethodsTest) suite run.
	self assert: (self failureMessageIn: result) equals: 'No package to check'
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMissingPackageAmongOthersFailsEveryTest [

	| result |
	result := (self scriptTestFor: { self fixturePackageName. self missingPackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: result failures size equals: 2
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMissingPackageFails [

	| result |
	result := (self scriptTestFor: { self missingPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testMisspelledPackageFails [

	| result |
	result := (self scriptTestFor: { 'SUnit-SmokeVerfier-Tests-Fixture' }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - running' }
SmokeVerifierTest >> testPackageNameDifferingByCaseIsChecked [
	"Package names are found ignoring case, so such a name is not misspelled: its scripts are run."

	| result |
	result := (self scriptTestFor: { self fixturePackageName asLowercase }) suite run.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #scriptBreaking )
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testPackageWithoutMethodFails [

	| result |
	PackageOrganizer default ensurePackage: self emptyPackageName.
	result := (self scriptTestFor: { self emptyPackageName }) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]

{ #category : 'tests - abstract' }
SmokeVerifierTest >> testProjectSubclassIsConcrete [

	self deny: (self scriptTestFor: { self fixturePackageName }) isAbstract.
	self deny: (self sampleInstanceTestFor: { self fixturePackageName }) isAbstract
]

{ #category : 'tests - running' }
SmokeVerifierTest >> testSampleInstanceTestChecksTheClassOfEveryInstance [

	| result |
	result := (self sampleInstanceTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #sampleInstanceOfAnotherClass )
]

{ #category : 'tests - running' }
SmokeVerifierTest >> testScriptTestRunsEveryScript [

	| result |
	result := (self scriptTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 2.
	self assert: (self failingMethodsIn: result) equals: #( #scriptBreaking )
]

{ #category : 'tests - abstract' }
SmokeVerifierTest >> testSmokeVerifierClassesAreAbstract [

	self assert: SmokePragmaMethodsTest isAbstract.
	self assert: SmokeScriptMethodsTest isAbstract.
	self assert: SmokeSampleInstanceMethodsTest isAbstract
]

{ #category : 'tests - checking nothing' }
SmokeVerifierTest >> testSubclassNotOverridingTheHookFails [

	| result |
	result := (self subclassOf: SmokeScriptMethodsTest named: #SmokeVerifierFixtureScriptMethodsTest) suite run.
	self assert: result runCount equals: 1.
	self assert: result failures size equals: 1
]
