"
I am the abstract smoke test of the class-side `<script>` methods: each script must run without error.

Like every subclass of `SmokePragmaMethodsTest`, I check nothing by myself: each project subclasses me in its own test package and overrides the class-side hook `packageNamesToCheck` to name its packages.

```
SmokeScriptMethodsTest << #MyProjectScriptMethodsTest
	package: 'MyProject-Tests'.

MyProjectScriptMethodsTest class >> packageNamesToCheck
	^ #( 'MyProject-Core' )
```

## Responsibility:

- Select the methods with `<script>` (`pragmaSelector`).
- Run each script in its own test and let any error fail it (`testScriptRunsWithoutError`).

## Collaborators:

- `SmokePragmaMethodsTest`: my superclass; it collects the methods, checks that there is something to check and runs them.

## Public API and Key Messages

- In a project subclass, override `packageNamesToCheck`, and optionally `excludedMethods` for scripts that must not run in a test (a modal dialog, a long computation).
- `MyProjectScriptMethodsTest suite run` runs one test per script.
"
Class {
	#name : 'SmokeScriptMethodsTest',
	#superclass : 'SmokePragmaMethodsTest',
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'private' }
SmokeScriptMethodsTest class >> isAbstract [

	^ self == SmokeScriptMethodsTest
]

{ #category : 'accessing' }
SmokeScriptMethodsTest class >> pragmaSelector [

	^ #script
]

{ #category : 'tests' }
SmokeScriptMethodsTest >> testScriptRunsWithoutError [

	self runMethod
]
