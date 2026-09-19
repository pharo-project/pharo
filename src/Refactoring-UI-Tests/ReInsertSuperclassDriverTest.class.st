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

	self assert: driver refactoring changes changes size equals: 2. "one because the subclasses got a new root the new subclass of the superclass"
	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 1.


	self assert: rbLeafClass superclass name equals: self newSuperclassName
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclassWithExistingClassNameInformsUser [

	| driver |
	driver := ReInsertSuperclassDriver new.

	self setUpDriver: driver.

	driver application: StApplicationForRefactorDriverMock new.

	driver
		superclass: self theClassToAddASuperclass;
		scopes: { testingEnvironment };
		subclass: self theClassToAddASuperclass name.

	driver runRefactoring.

	self assert: driver refactoring failedApplicabilityPreconditions size equals: 1.
	self assert: (driver application shownMessage includesSubstring: 'already exists').
	self assert: self theClassToAddASuperclass superclass name equals: #RBRootClassForInsertingSubclasses
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclassWithInvalidClassNameInformsUser [

	| driver |
	driver := ReInsertSuperclassDriver new.
	self setUpDriver: driver.
	driver application: StApplicationForRefactorDriverMock new.

	driver
		superclass: self theClassToAddASuperclass;
		scopes: { testingEnvironment };
		subclass: 'noCapital'.

	driver runRefactoring.

	self assert: driver refactoring failedApplicabilityPreconditions size equals: 1.
	self assert: (driver application shownMessage includesSubstring: 'is not a valid class name').
	self assert: self theClassToAddASuperclass superclass name equals: #RBRootClassForInsertingSubclasses
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> testInsertSuperclassWithoutExplicitParent [
	| driver rbLeafClass |
	
	driver := ReInsertSuperclassDriver new.
	
	self setUpDriver: driver.
	driver
		superclass: self theClassToAddASuperclass;
		scopes: { testingEnvironment };
		subclass: self newSuperclassName.
	rbLeafClass := driver superclass.
	
	self assert: rbLeafClass subclasses size equals: 0.
	
	driver runRefactoring.
	
	self assert: driver refactoring changes changes size equals: 2.
	self assert: rbLeafClass subclasses size equals: 0.
	self assert: rbLeafClass superclass subclasses size equals: 1.
	self assert: rbLeafClass superclass name equals: self newSuperclassName
]

{ #category : 'tests' }
ReInsertSuperclassDriverTest >> theClassToAddASuperclass [
	"Answer the <Class> which will be superclassed"
	
	^ ReLeaf1 
]
