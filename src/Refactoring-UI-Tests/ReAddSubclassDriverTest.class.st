Class {
	#name : 'ReAddSubclassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReAddSubclassDriverTest >> assertSubclassesOf: rbClass [


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
ReAddSubclassDriverTest >> newSubclassName [

	^ #ReChangeMock
]

{ #category : 'running' }
ReAddSubclassDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment classes: self superclassToAddSubclass withAllSubclasses 
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> superclassToAddSubclass [
	"Answer the <Class> which will be subclassed"
	
	^ RBRootClassForInsertingSubclasses 
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclass [

	| driver rbClass |

	driver := ReAddSubclassDriver new. 
	self setUpDriver: driver.
	driver 
		superclass: self superclassToAddSubclass;
		scopes: { testingEnvironment };
		subclass: self newSubclassName.
	
	rbClass := driver superclass.
	
	self assert: rbClass subclasses size equals: 2.

	driver runRefactoring.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 1.
		
	self assertSubclassesOf: rbClass
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassToDifferentPackage [

	| driver rbClass |

	driver := ReAddSubclassDriver new. 
	self setUpDriver: driver.

	driver	
		superclass: self superclassToAddSubclass name;
		subclass: self newSubclassName;
		scopes: { testingEnvironment };
		packageName: ReAddSubclassDriver packageName;	
		runRefactoring.
		
	rbClass := driver superclass.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 1.
	self
		assert: driver refactoring changes changes first package
		equals: ReAddSubclassDriver packageName.
		
	self assertSubclassesOf: rbClass
]
