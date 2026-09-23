Class {
	#name : 'ReInsertSubclassBeSiblingDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> assertSubclassesOf: rbClass [


	| expectedSubclassNames |
	expectedSubclassNames := OrderedCollection new. 
	expectedSubclassNames add: self newSubclassName;
		addAll:  (self superclassToAddSubclass subclasses collect: [ :each | each name ]).
		
	self assert: rbClass subclasses size equals: 3.
	self 
		assertCollection: (rbClass subclasses collect: [ :each | each name ]) 
		hasSameElements: expectedSubclassNames
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> newSubclassName [

	^ #ReChangeMock
]

{ #category : 'running' }
ReInsertSubclassBeSiblingDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment classes: self superclassToAddSubclass withAllSubclasses 
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> superclassToAddSubclass [
	"Answer the <Class> which will be subclassed"
	
	^ RBRootClassForInsertingSubclasses 
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> testAddSingleSubclass [

	| driver rbClass |
	driver := ReInsertSubclassDriver new.
	self setUpDriver: driver.
	driver
		beSibling;
		superclass: self superclassToAddSubclass;
		scopes: { testingEnvironment };
		subclass: self newSubclassName.

	rbClass := driver superclass.

	self assert: rbClass subclasses size equals: 2.

	driver runRefactoring.

	self assert: driver refactoring changes changes size equals: 1.

	self assertSubclassesOf: rbClass
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> testAddSingleSubclassToDifferentPackage [

	| driver rbClass |
	driver := ReInsertSubclassDriver new.
	self setUpDriver: driver.

	driver
		beSibling;
		superclass: self superclassToAddSubclass name;
		subclass: self newSubclassName;
		scopes: { testingEnvironment };
		packageName: RBAbstractClass packageName;
		runRefactoring.

	rbClass := driver superclass.

	self assert: driver refactoring changes changes size equals: 1.
	self assert: driver refactoring changes changes first package equals: RBAbstractClass packageName.

	self assertSubclassesOf: rbClass
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> testAddSingleSubclassWithExistingClassNameInformsUser [

	| driver |
	driver := ReInsertSubclassDriver new.

	self setUpDriver: driver.

	driver application: StApplicationForRefactorDriverMock new.

	driver
		beSibling;
		superclass: self superclassToAddSubclass;
		scopes: { testingEnvironment };
		subclass: self superclassToAddSubclass name.

	driver runRefactoring.

	self assert: driver refactoring failedApplicabilityPreconditions size equals: 1.
	self assert: (driver application shownMessage includesSubstring: 'already exists').
	self assert: self superclassToAddSubclass subclasses size equals: 2
]

{ #category : 'tests' }
ReInsertSubclassBeSiblingDriverTest >> testAddSingleSubclassWithInvalidClassNameInformsUser [

	| driver |
	driver := ReInsertSubclassDriver new.

	self setUpDriver: driver.

	driver application: StApplicationForRefactorDriverMock new.

	driver
		beSibling;
		superclass: self superclassToAddSubclass;
		scopes: { testingEnvironment };
		subclass: 'noCapital'.

	driver runRefactoring.

	self assert: driver refactoring failedApplicabilityPreconditions size equals: 1.
	self assert: (driver application shownMessage includesSubstring: 'is not a valid class name').
	self assert: self superclassToAddSubclass subclasses size equals: 2
]
