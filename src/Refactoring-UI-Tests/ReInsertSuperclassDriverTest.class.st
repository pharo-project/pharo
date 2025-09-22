Class {
	#name : 'ReInsertSuperclassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
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
	testingEnvironment := RBClassEnvironment classes: RBRootClassForInsertingSubclasses withAllSubclasses 
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
ReInsertSuperclassDriverTest >> theClassToAddASuperclass [
	"Answer the <Class> which will be subclassed"
	
	^ ReLeaf1 
]
