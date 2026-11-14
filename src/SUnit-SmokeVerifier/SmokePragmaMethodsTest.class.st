"
I am the abstract smoke test of the class-side methods tagged with a pragma: I run each of them in its own test.

My design is modular: I live in `SUnit-SmokeVerifier`, which only needs `SUnit-Core`, and I check nothing by myself. Each project defines its own subclass of `SmokeScriptMethodsTest` or `SmokeSampleInstanceMethodsTest`, in its own test package, and scopes it to its packages by overriding the class-side hook `packageNamesToCheck`. No core package is loaded to check a project, and no code is generated.

```
SmokeScriptMethodsTest << #MyProjectScriptMethodsTest
	package: 'MyProject-Tests'.

MyProjectScriptMethodsTest class >> packageNamesToCheck
	^ #( 'MyProject-Core' 'MyProject-UI' )
```

## Responsibility:

- Collect the class-side methods with my pragma in the packages to check, minus the excluded ones, and make one test per method (`testParameters`).
- Fail, never pass, when I would check nothing: no package to check, a package not found (misspelled or not loaded), or no method with my pragma.
- Run a method on its class (`runMethod`) for the tests of my subclasses.

## Collaborators:

- `ParametrizedTestCase`: my superclass; it builds one test per element of my `testParameters` matrix and sets `method`.
- `PackageOrganizer`: finds the packages to check and their methods, extension methods included.
- `SmokeScriptMethodsTest` and `SmokeSampleInstanceMethodsTest`: my subclasses, which define the pragma and the test.

## Public API and Key Messages

Class side, for project subclasses:
- `packageNamesToCheck`: the hook; answer the names of the packages to check. By default none, so the tests fail.
- `excludedMethods`: methods not to run, as strings such as `'MyClass class>>#resetAll'`. By default none.

Class side, for my subclasses:
- `pragmaSelector`: the pragma of the methods to check.

Instance side:
- `runMethod`: a project subclass can override it, for example to close the windows its scripts open: I know nothing about user interfaces. A method opening a modal dialog blocks the test: exclude it.

## Internal Representation

- `method`: the `CompiledMethod` checked by this test, set from the parameters.

### Implementation Points

- When there is no method to check, `testParameters` answers an empty matrix: `ParametrizedTestCase` then makes a single test, which fails in `setUp` instead of erring on a matrix without options.
- I, `SmokeScriptMethodsTest` and `SmokeSampleInstanceMethodsTest` are abstract (`isAbstract`); a project subclass is concrete.
"
Class {
	#name : 'SmokePragmaMethodsTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'method'
	],
	#category : 'SUnit-SmokeVerifier',
	#package : 'SUnit-SmokeVerifier'
}

{ #category : 'accessing' }
SmokePragmaMethodsTest class >> excludedMethods [
	"Answer the methods I do not run, as strings such as 'MyClass class>>#resetAll'. A project subclass overrides it to exclude methods that must not be run by a test."

	^ #(  )
]

{ #category : 'private' }
SmokePragmaMethodsTest class >> isAbstract [

	^ self == SmokePragmaMethodsTest
]

{ #category : 'accessing' }
SmokePragmaMethodsTest class >> methodsToCheck [
	"Answer the class-side methods with my pragma in the packages to check, except the excluded ones."

	| packages |
	packages := (self packageNamesToCheck collect: [ :each | self packageOrganizer packageNamed: each ifAbsent: [ nil ] ]) reject: [ :each | each isNil ].
	^ ((packages flatCollect: [ :package |
		package methods select: [ :each | each methodClass isClassSide and: [ each hasPragmaNamed: self pragmaSelector ] ] ])
		reject: [ :each | self excludedMethods includes: each printString ]) asArray
		sort: [ :a :b | a printString <= b printString ]
]

{ #category : 'accessing' }
SmokePragmaMethodsTest class >> missingPackageNames [
	"Answer the names of the packages to check that are not found: misspelled or not loaded."

	^ self packageNamesToCheck reject: [ :each | self packageOrganizer hasPackage: each ]
]

{ #category : 'accessing' }
SmokePragmaMethodsTest class >> packageNamesToCheck [
	"Hook: answer the names of the packages whose class-side methods I check. A project subclass overrides it; by default there is none, so the test fails."

	^ #(  )
]

{ #category : 'accessing' }
SmokePragmaMethodsTest class >> pragmaSelector [
	"Answer the pragma of the class-side methods I check."

	^ self subclassResponsibility
]

{ #category : 'building suites' }
SmokePragmaMethodsTest class >> testParameters [
	"Answer one option per method to check, or an empty matrix when there is none: ParametrizedTestCase>>#setUp cannot use a matrix without options."

	| methods |
	methods := self methodsToCheck.
	methods ifEmpty: [ ^ ParametrizedTestMatrix new ].
	^ ParametrizedTestMatrix new
		forSelector: #method addOptions: methods;
		yourself
]

{ #category : 'running' }
SmokePragmaMethodsTest >> failIfNoMethodToCheck [
	"Fail when the packages to check have no class-side method with my pragma: ParametrizedTestCase then makes a single test without method."

	method ifNil: [ self fail: ('No class-side method with <{1}> in the packages to check' format: { self class pragmaSelector }) ]
]

{ #category : 'running' }
SmokePragmaMethodsTest >> failIfNoPackageToCheck [
	"Fail when my class does not name any package: a test checking nothing must not pass."

	self class packageNamesToCheck ifEmpty: [ self fail: 'No package to check' ]
]

{ #category : 'running' }
SmokePragmaMethodsTest >> failIfPackageIsNotFound [
	"Fail when a package to check is not found, because its name is misspelled or it is not loaded: its methods would silently not be checked. Names are compared ignoring case."

	self class missingPackageNames ifNotEmpty: [ :missing | self fail: ('Packages to check not found, misspelled or not loaded: {1}' format: { ', ' join: missing }) ]
]

{ #category : 'accessing' }
SmokePragmaMethodsTest >> method [

	^ method
]

{ #category : 'accessing' }
SmokePragmaMethodsTest >> method: aCompiledMethod [

	method := aCompiledMethod
]

{ #category : 'running' }
SmokePragmaMethodsTest >> runMethod [
	"Answer the result of running my method on its class. A project subclass can override it, for example to close the windows the method opens."

	^ method valueWithReceiver: method methodClass instanceSide
]

{ #category : 'running' }
SmokePragmaMethodsTest >> setUp [

	super setUp.
	self failIfNoPackageToCheck.
	self failIfPackageIsNotFound.
	self failIfNoMethodToCheck
]
