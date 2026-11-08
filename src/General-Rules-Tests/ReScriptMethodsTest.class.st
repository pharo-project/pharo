"
I run every class-side method with the pragma `<script>` of the packages to check, and fail when one of them raises an error.

## Responsibility:

- Answer `#script` as the pragma of the methods to run.
- Run each script, closing the windows it opens.

## Collaborators:

- `RePragmaMethodsTest`: finds the methods, runs them and makes copies of me.

## Public API and Key Messages

- `testScriptRunsWithoutError` runs the script of the test.
- Use me through a copy for a project:

```
ReScriptMethodsTest copyNamed: #MyProjectScriptMethodsTest forPackagesNamed: #( 'MyProject' ) in: 'MyProject-Tests'
```

### Implementation Points

- A script may change the image (reset, clean up, generate code): list such scripts in `excludedMethods` of the copy.
"
Class {
	#name : 'ReScriptMethodsTest',
	#superclass : 'RePragmaMethodsTest',
	#category : 'General-Rules-Tests',
	#package : 'General-Rules-Tests'
}

{ #category : 'private' }
ReScriptMethodsTest class >> isAbstract [

	^ self == ReScriptMethodsTest
]

{ #category : 'accessing' }
ReScriptMethodsTest class >> pragmaSelector [

	^ #script
]

{ #category : 'tests' }
ReScriptMethodsTest >> testScriptRunsWithoutError [

	self runMethod
]
