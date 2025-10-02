Class {
	#name : 'ReInsertSubclassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment',
		'systemEnvironment'
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
	testingEnvironment := RBClassEnvironment classes: self superclassToAddSubclass withAllSubclasses.
	systemEnvironment := RBBrowserEnvironment default
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
ReInsertSubclassDriverTest >> testInsertSubclassChoosesASmallScopeIfAvailable [

	| driver aClass |

	driver := ReInsertSubclassDriver new.
	aClass := self superclassToAddSubclass.
	self setUpDriver: driver.
	driver superclass: aClass.
	driver scopes: { systemEnvironment . testingEnvironment }.
	
	self deny: driver model environment isSystem
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

{ #category : 'tests' }
ReInsertSubclassDriverTest >> testInsertSubclasssMissingAValidScopeFails [

	| driver aClass invalidEnvironment |

	driver := ReInsertSuperclassDriver new.
	aClass := self superclassToAddSubclass.
	self setUpDriver: driver.
	driver superclass: aClass.
	invalidEnvironment := RBClassEnvironment classes: { aClass subclasses first }.
	
	"The environment does not include aClass, the driver should fail."
	self should: [ driver scopes: { invalidEnvironment } ] raise: RefactoringDriverError .
]
