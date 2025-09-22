Class {
	#name : 'ReInsertSubclassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReInsertSubclassDriverTest >> newSubclassName [

	^ #ReChangeMock
]

{ #category : 'running' }
ReInsertSubclassDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment classes: self superclassToAddSubclass withAllSubclasses 
]

{ #category : 'tests' }
ReInsertSubclassDriverTest >> superclassToAddSubclass [
	"Answer the <Class> which will be subclassed"
	
	^ RBRootClassForInsertingSubclasses 
]

{ #category : 'tests' }
ReInsertSubclassDriverTest >> testInsertSubclass [

	| driver rbClass |

	driver := ReInsertSubclassDriver new. 
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
		equals: 3.
		
	"one because the subclasses got a new root the new subclass of the superclass"
	self assert: rbClass subclasses size equals: 1.
	self assert: rbClass subclasses first subclasses size equals: 2.
	
	self assert: rbClass subclasses first name equals: self newSubclassName
]

{ #category : 'tests' }
ReInsertSubclassDriverTest >> testInsertSubclassInADifferentPackage [

	| driver rbClass |

	driver := ReInsertSubclassDriver new. 
	self setUpDriver: driver.
	
	driver 
		superclass: self superclassToAddSubclass;
		scopes: { testingEnvironment };
		packageName: ReAddSubclassDriver packageName;	
		subclass: self newSubclassName.
	
	rbClass := driver superclass.
	
	self assert: rbClass subclasses size equals: 2.

	driver runRefactoring.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 3.
		
	"one because the subclasses got a new root the new subclass of the superclass"
	self assert: rbClass subclasses size equals: 1.
	self assert: rbClass subclasses first subclasses size equals: 2.
	
	self assert: rbClass subclasses first name equals: self newSubclassName.
	

	self
		assert: driver refactoring changes changes first package
		equals: ReAddSubclassDriver packageName
]
