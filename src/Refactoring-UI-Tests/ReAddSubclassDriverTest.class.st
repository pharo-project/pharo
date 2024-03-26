Class {
	#name : 'ReAddSubclassDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReAddSubclassDriverTest >> newSubclassName [

	^ #RBRefactoringChangeMock
]

{ #category : 'running' }
ReAddSubclassDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> superclassToAddSubclass [
	"Answer the <Class> which will be subclassed"
	
	^ RBRootClassForInsertingSubclasses 
]

{ #category : 'running' }
ReAddSubclassDriverTest >> tearDown [

	(testingEnvironment notNil and: [ testingEnvironment classNames includes: self newSubclassName ])
		ifTrue: [ testingEnvironment removeClass: self newSubclassName ].
	super tearDown.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclass [

	| driver rbClass |

	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	self setUpDriver: driver.	
	driver runRefactoring.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 1.
	self
		assertCollection: (rbClass subclasses collect: #name)
		hasSameElements: { self newSubclassName }.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassToDifferentPackage [

	| driver |

	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			package: ReAddSubclassDriver package;
			yourself.
	
	self setUpDriver: driver.
	driver runRefactoring.
	
	self 
		assert: driver refactoring changes changes size 
		equals: 1.
	self
		assert: driver refactoring changes changes anyOne package
		equals: ReAddSubclassDriver package name
]
