Class {
	#name : 'ReInsertSuperclassDriverTest',
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
ReInsertSuperclassDriverTest >> newSubclassName [

	^ #ReChangeMock
]

{ #category : 'running' }
ReInsertSuperclassDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment classes: RBRootClassForInsertingSubclasses withAllSubclasses.
	systemEnvironment := RBBrowserEnvironment default
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclass [

	| driver rbLeafClass |

	driver := ReInsertSuperclassDriver new. 
	self setUpDriver: driver.
	
	driver 
		superclass: self theClassToAddASuperclass ;
		scopes: { testingEnvironment };
		subclass: self newSubclassName.
		
	rbLeafClass := driver superclass.
	
	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 2.

	driver runRefactoring.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 3.
		
	"one because the subclasses got a new root the new subclass of the superclass"
	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 2.

	
	self assert: rbLeafClass superclass name equals: self newSubclassName.
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclassChoosesASmallScopeIfAvailable [

	| driver aClass |

	driver := ReInsertSuperclassDriver new.
	aClass := self theClassToAddASuperclass.
	self setUpDriver: driver.
	driver superclass: aClass.
	driver scopes: { systemEnvironment . testingEnvironment }.
	
	self deny: driver model environment isSystem
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclassMissingAValidScopeFails [

	| driver aClass invalidEnvironment |

	driver := ReInsertSuperclassDriver new.
	aClass := self theClassToAddASuperclass.
	self setUpDriver: driver.
	driver superclass: aClass.
	invalidEnvironment := RBClassEnvironment classes: { aClass }.
	
	"The environment does not include aClass superclass, the driver should fail."
	self should: [ driver scopes: { invalidEnvironment } ] raise: RefactoringDriverError .
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> theClassToAddASuperclass [
	"Answer the <Class> which will be subclassed"
	
	^ ReLeaf1 
]
