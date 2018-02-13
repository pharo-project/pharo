"
A ManifestBuilderTest is a class to test the behavior of ManifestBuilder
"
Class {
	#name : #BuilderManifestTest,
	#superclass : #TestCase,
	#category : #'Manifest-Tests'
}

{ #category : #running }
BuilderManifestTest >> setUp [
	
	| cl |
	super setUp.
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].

]

{ #category : #running }
BuilderManifestTest >> tearDown [
	
	| cl |
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].
	super tearDown		

]

{ #category : #tests }
BuilderManifestTest >> testAddAllFalsePositive [
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0FalsePositive size = 2).
	
	manifestBuilder removeAllFalsePositive: array of: 'test' version: 0. 
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	"manifestBuilder manifest class removeSelector: (manifestBuilder selectorFalsePositiveOf: 'test' version: 0)"
]

{ #category : #tests }
BuilderManifestTest >> testAddAllToDo [
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllToDo: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllToDo: array of: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0TODO size = 2).

]

{ #category : #tests }
BuilderManifestTest >> testAddClass [
	
	| manifestBuilder|
	
	manifestBuilder := TheManifestBuilder of: MFClassA .
	
	self deny: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).
	
	manifestBuilder addRejectClass: MFClassA..
	
	self assert: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).

	manifestBuilder removeRejectClass: MFClassA.	.

	self deny: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).
	
]

{ #category : #tests }
BuilderManifestTest >> testAddFalsePositive [
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder addFalsePositive: cl of: 'test' version: 0. 
	manifestBuilder addFalsePositive: mth of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder removeFalsePositive: cl of: 'test' version: 0. 
	manifestBuilder removeFalsePositive: mth of: 'test' version: 0. 
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder falsePositiveOf: 'test'version: 0) anySatisfy: [:each| each = mth]).

]

{ #category : #tests }
BuilderManifestTest >> testAddRule [
	
	| manifestBuilder|
	
	manifestBuilder := TheManifestBuilder of: MFClassA .
	
	self deny: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).
	
	manifestBuilder addRejectRule: 0.
	
	self assert: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).

	manifestBuilder removeRejectRule: 0.	

	self deny: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).
	
]

{ #category : #tests }
BuilderManifestTest >> testAddToDo [
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
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
	self deny: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
]

{ #category : #tests }
BuilderManifestTest >> testCleanUpFP [
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	MFClassA compile:  'foo'.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive: (MFClassA>>#foo) of:  'test' version: 0. 
	
	self assert: (manifestBuilder containsFalsePositive: (MFClassA>>#foo) onRule: 'test' version: 0).
	
	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0 ) size = 0) .

]

{ #category : #tests }
BuilderManifestTest >> testCleanUpTD [
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	MFClassA compile:  'foo'.
	manifestBuilder installToDoOf: 'test' version: 0.
	manifestBuilder addToDo: (MFClassA>>#foo) of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsToDo: (MFClassA>>#foo) onRule: 'test' version: 0).
	
	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.
	
	self assert: ((manifestBuilder toDoOf: 'test' version: 0 ) size = 0) .

]

{ #category : #tests }
BuilderManifestTest >> testContainsFalsePositive [
	
	| manifestBuilder |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsFalsePositive: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsFalsePositive: MFClassB onRule: 'test' version: 0).
	
	
	
]

{ #category : #tests }
BuilderManifestTest >> testContainsToDo [
	
	| manifestBuilder |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	manifestBuilder addToDo: MFClassA of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsToDo: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsToDo: MFClassB onRule: 'test' version: 0).
	
	
	
]

{ #category : #tests }
BuilderManifestTest >> testCreationManifest [
	| manifestBuilder cl |
	manifestBuilder := TheManifestBuilder new.
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].
	self assert: (manifestBuilder manifestOf: MFClassA ) isNil.
	self assert: (manifestBuilder createManifestOf: MFClassA) notNil.
	self assert: (manifestBuilder manifestOf: MFClassA) notNil
]

{ #category : #tests }
BuilderManifestTest >> testCreationManifestOn [
	| manifestBuilder cl |
	manifestBuilder := TheManifestBuilder new.
	cl := Smalltalk globals at: #ManifestManifestResourcesTests ifAbsent: [ nil ].
	cl
		ifNotNil: [ 
			cl
				removeFromChanges;
				removeFromSystemUnlogged ].
	self assert: (manifestBuilder manifestOf: MFClassA ) isNil.
	self assert: (TheManifestBuilder of: MFClassA) notNil.
	self assert: (manifestBuilder manifestOf: MFClassA) notNil
]

{ #category : #tests }
BuilderManifestTest >> testDateOfFalsePositive [
	
	| manifestBuilder date1 date2 |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	date1 :=  DateAndTime current.
	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0. 
	date2 :=  DateAndTime current.
	
	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) >= date1 .
	self assert: (manifestBuilder dateOfFalsePositive: MFClassA onRule: 'test' version: 0) <= date2 .
	
	
	
	
]

{ #category : #tests }
BuilderManifestTest >> testDateOfToDo [
	
	| manifestBuilder date1 date2 |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	date1 :=  DateAndTime current.
	manifestBuilder addToDo: MFClassA of: 'test' version: 0. 
	date2 :=  DateAndTime current.
	
	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) >= date1 .
	self assert: (manifestBuilder dateOfToDo: MFClassA onRule: 'test' version: 0) <= date2 .
	
	
	
	
]

{ #category : #tests }
BuilderManifestTest >> testInstallFalsePositive [
	| manifestBuilder |
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	self deny: (manifestBuilder hasFalsePositiveOf: 'test' version: 0).
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	self assert: (manifestBuilder hasFalsePositiveOf: 'test' version: 0).
	self assert: (manifestBuilder falsePositiveOf: 'test' version: 0) notNil.

]

{ #category : #tests }
BuilderManifestTest >> testInstallToDo [
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	self deny: (manifestBuilder hasToDoOf: 'test' version: 0).
	manifestBuilder installToDoOf: 'test' version: 0.
	self assert: (manifestBuilder hasToDoOf: 'test' version: 0).
	self assert: (manifestBuilder toDoOf: 'test' version: 0) notNil.
	
]

{ #category : #tests }
BuilderManifestTest >> testIsClassAManifest [
		
	self deny: Point isManifest.
	TheManifestBuilder of: MFClassA. 
	self assert: ( (Smalltalk at: #ManifestManifestResourcesTests) isManifest)
]

{ #category : #tests }
BuilderManifestTest >> testIsFalsePositive [
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := cl >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder addRejectClass: MFClassA.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder removeRejectClass: MFClassA.	.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder addRejectRule: 'test'.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder removeRejectRule: 'test'.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive:  mth of: 'test' version: 0.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	
]

{ #category : #tests }
BuilderManifestTest >> testResetFalsePositive [
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder resetFalsePositiveOf: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0FalsePositive size = 0).
	
	
]

{ #category : #tests }
BuilderManifestTest >> testResetToDo [
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installToDoOf: 'test' version: 0.
	
	manifestBuilder addAllToDo: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder toDoOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder resetToDoOf: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0TODO size = 0).
	
	
]
