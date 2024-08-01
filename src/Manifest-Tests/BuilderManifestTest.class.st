"
A ManifestBuilderTest is a class to test the behavior of ManifestBuilder
"
Class {
	#name : 'BuilderManifestTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'manifestBuilder'
	],
	#category : 'Manifest-Tests-Base',
	#package : 'Manifest-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
BuilderManifestTest >> setUp [

	super setUp.
	manifestBuilder := TheManifestBuilder of: MFClassA
]

{ #category : 'running' }
BuilderManifestTest >> tearDown [

	testingEnvironment at: #ManifestManifestResourcesTests ifPresent: [ :class | class removeFromSystem ].
	super tearDown
]

{ #category : 'tests' }
BuilderManifestTest >> testAddAllFalsePositive [

	<ignoreNotImplementedSelectors: #( ruletestV0FalsePositive )>
	| array |
	array := { MFClassA . MFClassB }.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.

	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder addAllFalsePositive: array of: 'test' version: 0.

	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder addAllFalsePositive: array of: 'test' version: 0.

	self assert: manifestBuilder manifest ruletestV0FalsePositive size equals: 2.

	manifestBuilder removeAllFalsePositive: array of: 'test' version: 0.

	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ])

	"manifestBuilder manifest class removeSelector: (manifestBuilder selectorFalsePositiveOf: 'test' version: 0)"
]

{ #category : 'tests' }
BuilderManifestTest >> testAddAllToDo [
	| array |
	<ignoreNotImplementedSelectors: #(ruletestV0TODO)>
	array := {MFClassA . MFClassB}.
	manifestBuilder installToDoOf: 'test' version: 0.

	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder addAllToDo: array of: 'test' version: 0.

	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder addAllToDo: array of: 'test' version: 0.

	self assert: manifestBuilder manifest ruletestV0TODO size equals: 2
]

{ #category : 'tests' }
BuilderManifestTest >> testAddClass [

	self deny: (manifestBuilder rejectClasses anySatisfy: [ :each | each = MFClassA ]).

	manifestBuilder addRejectClass: MFClassA.

	self assert: (manifestBuilder rejectClasses anySatisfy: [ :each | each = MFClassA ]).

	manifestBuilder removeRejectClass: MFClassA.

	self deny: (manifestBuilder rejectClasses anySatisfy: [ :each | each = MFClassA ])
]

{ #category : 'tests' }
BuilderManifestTest >> testAddFalsePositive [

	| cl mth |
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.

	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = cl ]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = mth ]).

	manifestBuilder addFalsePositive: cl of: 'test' version: 0.
	manifestBuilder addFalsePositive: mth of: 'test' version: 0.

	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = cl ]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = mth ]).

	manifestBuilder removeFalsePositive: cl of: 'test' version: 0.
	manifestBuilder removeFalsePositive: mth of: 'test' version: 0.

	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = cl ]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = mth ])
]

{ #category : 'tests' }
BuilderManifestTest >> testAddRule [

	self deny: (manifestBuilder rejectRules anySatisfy: [ :each | each = 0 ]).

	manifestBuilder addRejectRule: 0.

	self assert: (manifestBuilder rejectRules anySatisfy: [ :each | each = 0 ]).

	manifestBuilder removeRejectRule: 0.

	self deny: (manifestBuilder rejectRules anySatisfy: [ :each | each = 0 ])
]

{ #category : 'tests' }
BuilderManifestTest >> testAddToDo [

	| cl mth|
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder  installToDoOf: 'test' version: 0.

	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).

	manifestBuilder addToDo: cl of: 'test' version: 0.
	manifestBuilder addToDo: mth of: 'test' version: 0.

	self assert: ((manifestBuilder  toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self assert: ((manifestBuilder  toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).

	manifestBuilder removeToDo: cl of: 'test' version: 0.
	manifestBuilder removeToDo: mth of: 'test' version: 0.

	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth])
]

{ #category : 'tests' }
BuilderManifestTest >> testCleanUpFP [
	MFClassA compile: 'foo'.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive: MFClassA >> #foo of: 'test' version: 0.

	self assert: (manifestBuilder containsFalsePositive: MFClassA >> #foo onRule: 'test' version: 0).

	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.

	self assert: (manifestBuilder falsePositiveOf: 'test' version: 0) size equals: 0
]

{ #category : 'tests' }
BuilderManifestTest >> testCleanUpTD [
	MFClassA compile: 'foo'.
	manifestBuilder installToDoOf: 'test' version: 0.
	manifestBuilder addToDo: MFClassA >> #foo of: 'test' version: 0.

	self assert: (manifestBuilder containsToDo: MFClassA >> #foo onRule: 'test' version: 0).

	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.

	self assert: (manifestBuilder toDoOf: 'test' version: 0) size equals: 0
]

{ #category : 'tests' }
BuilderManifestTest >> testContainsFalsePositive [

	manifestBuilder  installFalsePositiveOf: 'test' version: 0.

	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0.

	self assert: (manifestBuilder containsFalsePositive: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsFalsePositive: MFClassB onRule: 'test' version: 0)
]

{ #category : 'tests' }
BuilderManifestTest >> testContainsToDo [

	manifestBuilder  installToDoOf: 'test' version: 0.

	manifestBuilder addToDo: MFClassA of: 'test' version: 0.

	self assert: (manifestBuilder containsToDo: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsToDo: MFClassB onRule: 'test' version: 0)
]

{ #category : 'tests' }
BuilderManifestTest >> testCreationManifest [

	manifestBuilder := TheManifestBuilder new.
	testingEnvironment at: #ManifestManifestResourcesTests ifPresent: [ :class | class removeFromSystem ].
	self assert: (manifestBuilder manifestOf: MFClassA) isNil.
	self assert: (manifestBuilder createManifestOf: MFClassA) isNotNil.
	self assert: (manifestBuilder manifestOf: MFClassA) isNotNil
]

{ #category : 'tests' }
BuilderManifestTest >> testCreationManifestOn [

	manifestBuilder := TheManifestBuilder new.
	testingEnvironment at: #ManifestManifestResourcesTests ifPresent: [ :class | class removeFromSystem ].
	self assert: (manifestBuilder manifestOf: MFClassA) isNil.
	self assert: (TheManifestBuilder of: MFClassA) isNotNil.
	self assert: (manifestBuilder manifestOf: MFClassA) isNotNil
]

{ #category : 'tests' }
BuilderManifestTest >> testDateOfFalsePositive [

	| date1 date2 |
	manifestBuilder installFalsePositiveOf: 'test' version: 0.

	date1 := DateAndTime current.
	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0.
	date2 := DateAndTime current.

	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) >= date1.
	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) <= date2
]

{ #category : 'tests' }
BuilderManifestTest >> testDateOfToDo [

	| date1 date2 |
	manifestBuilder installToDoOf: 'test' version: 0.

	date1 := DateAndTime current.
	manifestBuilder addToDo: MFClassA of: 'test' version: 0.
	date2 := DateAndTime current.

	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) >= date1.
	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) <= date2
]

{ #category : 'tests' }
BuilderManifestTest >> testInstallFalsePositive [

	self deny: (manifestBuilder hasFalsePositiveOf: 'test' version: 0).
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	self assert: (manifestBuilder hasFalsePositiveOf: 'test' version: 0).
	self assert: (manifestBuilder falsePositiveOf: 'test' version: 0) isNotNil
]

{ #category : 'tests' }
BuilderManifestTest >> testInstallToDo [

	self deny: (manifestBuilder hasToDoOf: 'test' version: 0).
	manifestBuilder installToDoOf: 'test' version: 0.
	self assert: (manifestBuilder hasToDoOf: 'test' version: 0).
	self assert: (manifestBuilder toDoOf: 'test' version: 0) isNotNil
]

{ #category : 'tests' }
BuilderManifestTest >> testIsClassAManifest [

	self deny: Point isManifest.
	self assert: (testingEnvironment at: #ManifestManifestResourcesTests) isManifest
]

{ #category : 'tests' }
BuilderManifestTest >> testIsFalsePositive [

	| cl mth |
	cl := MFClassA.
	mth := cl >> #method.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0).

	manifestBuilder addRejectClass: MFClassA.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0).

	manifestBuilder removeRejectClass: MFClassA.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0).

	manifestBuilder addRejectRule: 'test'.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0).

	manifestBuilder removeRejectRule: 'test'.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0).

	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive: mth of: 'test' version: 0.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version: 0)
]

{ #category : 'tests' }
BuilderManifestTest >> testResetFalsePositive [

	<ignoreNotImplementedSelectors: #( ruletestV0FalsePositive )>
	| array |
	array := { MFClassA . MFClassB }.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.

	manifestBuilder addAllFalsePositive: array of: 'test' version: 0.

	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder resetFalsePositiveOf: 'test' version: 0.

	self assert: manifestBuilder manifest ruletestV0FalsePositive size equals: 0
]

{ #category : 'tests' }
BuilderManifestTest >> testResetToDo [

	<ignoreNotImplementedSelectors: #( ruletestV0TODO )>
	| array |
	array := { MFClassA . MFClassB }.
	manifestBuilder installToDoOf: 'test' version: 0.

	manifestBuilder addAllToDo: array of: 'test' version: 0.

	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassA ]).
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [ :each | each = MFClassB ]).

	manifestBuilder resetToDoOf: 'test' version: 0.

	self assert: manifestBuilder manifest ruletestV0TODO size equals: 0
]
