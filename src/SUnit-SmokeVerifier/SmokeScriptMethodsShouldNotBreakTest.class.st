"
I am the abstract smoke test of the class-side `<script>` and `<example>` methods: each must run without error.

Like every subclass of `SmokePragmaMethodsTest`, I check nothing by myself: each project subclasses me in its own test package and overrides the class-side hook `packageNamesToCheck` to name its packages.

```
SmokeScriptMethodsShouldNotBreakTest << #MyProjectScriptMethodsTest
	package: 'MyProject-Tests'.

MyProjectScriptMethodsTest class >> packageNamesToCheck
	^ #( 'MyProject-Core' )
```

## Responsibility:

- Select the methods with `<script>` or `<example>` (`pragmaSelectors`).
- Run each script in its own test and let any error fail it (`testScriptRunsWithoutError`).

## Collaborators:

- `SmokePragmaMethodsTest`: my superclass; it collects the methods, checks that there is something to check and runs them.

## Public API and Key Messages

- In a project subclass, override `packageNamesToCheck`, and optionally `excludedMethods` for scripts that must not run in a test (a modal dialog, a long computation).
- `MyProjectScriptMethodsTest suite run` runs one test per script.
"
Class {
	#name : 'SmokeScriptMethodsShouldNotBreakTest',
	#superclass : 'SmokePragmaMethodsTest',
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'private' }
SmokeScriptMethodsShouldNotBreakTest class >> isAbstract [

	^ self == SmokeScriptMethodsShouldNotBreakTest
]

{ #category : 'accessing' }
SmokeScriptMethodsShouldNotBreakTest class >> pragmaSelectors [

	^ #(script example)
]

{ #category : 'tests' }
SmokeScriptMethodsShouldNotBreakTest >> testScriptRunsWithoutError [
	"Check that all the methods with a script or example pragma run without error."
	
	self shouldnt: [ self runMethod ] raise: Error
]
