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

	^ #RBNewSubclassForAddSubclassDriverTest
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

	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
	
	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	driver runRefactoring.
	
	self
		assert: (rbClass subclasses collect: #name)
		equals: self newSubclassName.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassToDifferentPackage [

	| driver rbClass |

	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
	
	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	driver runRefactoring.
	
	self
		assert: (rbClass subclasses collect: #name)
		equals: self newSubclassName.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassWithExistingName [

	| driver rbClass |

	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
	
	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	driver runRefactoring.
	
	self
		assert: (rbClass subclasses collect: #name)
		equals: self newSubclassName.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassWithExistingSubclasses [

	| driver rbClass |

	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
	
	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	driver runRefactoring.
	
	self
		assert: (rbClass subclasses collect: #name)
		equals: self newSubclassName.
]

{ #category : 'tests' }
ReAddSubclassDriverTest >> testAddSingleSubclassWithInvalidName [

	| driver rbClass |

	testingEnvironment := RBClassEnvironment class: self superclassToAddSubclass. 
	
	driver := ReAddSubclassDriver new
			scopes: { testingEnvironment } subclass: self newSubclassName;
			yourself.
	rbClass := driver superclass.
	self deny: rbClass hasSubclasses.
	
	driver runRefactoring.
	
	self
		assert: (rbClass subclasses collect: #name)
		equals: self newSubclassName.
]
