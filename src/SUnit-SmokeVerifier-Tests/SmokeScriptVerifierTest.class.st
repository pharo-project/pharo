"
I verify `SmokeScriptMethodsShouldNotBreakTest`: every class-side `<script>` and `<example>` method of the packages to check must run without error.

## Responsibility:

- Fill the fixture class with scripts and examples that run or break, and with an instance-side script, which must be ignored.
- Check that a project subclass runs the four class-side methods and fails on exactly the two breaking ones.
- Check that package names are found ignoring case.

## Collaborators:

- `SmokeVerifierTest`: my superclass; it makes the fixture class and holds the checks common to every smoke test.
- `SmokeScriptMethodsShouldNotBreakTest`: the smoke test I subclass and run.

## Public API and Key Messages

- `smokeTestClass`, `compileFixtureMethods`, `fixtureMethodCount`, `fixtureMethodToExclude`: the hooks of `SmokeVerifierTest`.
- Run my tests:

```
SmokeScriptVerifierTest suite run
```
"
Class {
	#name : 'SmokeScriptVerifierTest',
	#superclass : 'SmokeVerifierTest',
	#category : 'SUnit-SmokeVerifier-Tests',
	#package : 'SUnit-SmokeVerifier-Tests'
}

{ #category : 'hooks' }
SmokeScriptVerifierTest >> compileFixtureMethods [
	"Compile scripts and examples that run or break, and an instance-side script, which is not run."

	fixtureClass
		compile: 'instanceSideScript
	<script>
	^ 1 / 0'
		classified: #fixture.
	fixtureClass class
		compile: 'scriptRunning
	<script>
	^ 3 + 4'
		classified: #fixture.
	fixtureClass class
		compile: 'scriptBreaking
	<script>
	^ 1 / 0'
		classified: #fixture.
	fixtureClass class
		compile: 'exampleRunning
	<example>
	^ Object new'
		classified: #fixture.
	fixtureClass class
		compile: 'exampleBreaking
	<example>
	^ 1 / 0'
		classified: #fixture
]

{ #category : 'hooks' }
SmokeScriptVerifierTest >> fixtureMethodToExclude [

	^ 'SmokeVerifierFixtureClass class>>#scriptBreaking'
]

{ #category : 'hooks' }
SmokeScriptVerifierTest >> smokeTestClass [

	^ SmokeScriptMethodsShouldNotBreakTest
]

{ #category : 'tests - running' }
SmokeScriptVerifierTest >> testPackageNameDifferingByCaseIsChecked [
	"Package names are found ignoring case, so such a name is not misspelled: its scripts are run."

	| result |
	result := (self smokeTestFor: { self fixturePackageName asLowercase }) suite run.
	self assert: result runCount equals: 4.
	self assert: (self failingMethodsIn: result) size equals: 2.
	self assert: ((self failingMethodsIn: result) includes: #scriptBreaking).
	self assert: ((self failingMethodsIn: result) includes: #exampleBreaking)
]

{ #category : 'tests - running' }
SmokeScriptVerifierTest >> testScriptTestRunsEveryScript [

	| result |
	result := (self smokeTestFor: { self fixturePackageName }) suite run.
	self assert: result runCount equals: 4.
	self assert: (self failingMethodsIn: result) size equals: 2.
	self assert: ((self failingMethodsIn: result) includes: #scriptBreaking).
	self assert: ((self failingMethodsIn: result) includes: #exampleBreaking)
]
