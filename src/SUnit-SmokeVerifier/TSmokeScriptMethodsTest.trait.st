"
I am the smoke test running every class-side method with the pragma `<script>` of the packages to check: a test fails when its script raises an error.

## Responsibility:

- Answer `#script` as the pragma of the methods to run.
- Run each script.

## Collaborators:

- `TSmokePragmaMethodsTest`: finds the methods, runs them and fails when nothing is checked.
- `ProjectSmokeVerifierGenerator`: generates the class of a project using me (`generateScriptTest`).

## Public API and Key Messages

- `testScriptRunsWithoutError` runs the script of the test.

```
(ProjectSmokeVerifierGenerator new
	projectName: 'MyProject';
	packageNames: #( 'MyProject' );
	testPackageName: 'MyProject-Tests';
	generateScriptTest) suite run
```

### Implementation Points

- A script may change the image (reset, clean up, generate code): list such scripts in the class-side `excludedMethods` of the generated class.
"
Trait {
	#name : 'TSmokeScriptMethodsTest',
	#traits : 'TSmokePragmaMethodsTest',
	#classTraits : 'TSmokePragmaMethodsTest classTrait',
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'accessing' }
TSmokeScriptMethodsTest classSide >> pragmaSelector [

	^ #script
]

{ #category : 'tests' }
TSmokeScriptMethodsTest >> testScriptRunsWithoutError [

	self runMethod
]
