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
ReInsertSuperclassDriverTest >> newSuperclassName [

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
		superclass: self theClassToAddASuperclass;
		superclassParent: self theClassToAddASuperclass superclass;
		scopes: { testingEnvironment };
		subclass: self newSuperclassName.

	rbLeafClass := driver superclass.

	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 2.

	driver runRefactoring.

	self assert: driver refactoring changes changes size equals: 3. "one because the subclasses got a new root the new subclass of the superclass"
	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 2.


	self assert: rbLeafClass superclass name equals: self newSuperclassName
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> theClassToAddASuperclass [
	"Answer the <Class> which will be superclassed"
	
	^ ReLeaf1 
]
