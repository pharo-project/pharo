"
I am an abstract test for Spec widgets.

I use classToTest to initialize an instance. And I can keep the created windows in a variable that I clean after tests.

I provide openInstance and openInstance: to open it. These methods make me keep the created window in the variable.

- testOpenWithSpec tests a call at openWhitSpec, it test the minimal use but can fail in some widgets needing to be initialized.
- testExample tests the class side method example if exist, example should be a representative use.
"
Class {
	#name : #SpecTestCase,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'testedInstance'
	],
	#category : #'Spec-Tests-Utils'
}

{ #category : #testing }
SpecTestCase class >> isAbstract [
	^ self name = #SpecTestCase
]

{ #category : #testing }
SpecTestCase class >> shouldInheritSelectors [
	^ true
]

{ #category : #running }
SpecTestCase >> classToTest [
	self subclassResponsibility
]

{ #category : #running }
SpecTestCase >> initializeTestedInstance [
]

{ #category : #running }
SpecTestCase >> openInstance [
	window := testedInstance openWithSpec
]

{ #category : #running }
SpecTestCase >> openInstance: aLayoutSelector [
	window := testedInstance openWithSpec: aLayoutSelector
]

{ #category : #running }
SpecTestCase >> setUp [
	super setUp.
	testedInstance := self classToTest new.
	self initializeTestedInstance
]

{ #category : #running }
SpecTestCase >> tearDown [
	
	window ifNotNil: [ window delete ].
	super tearDown.
]

{ #category : #tests }
SpecTestCase >> testExample [
	[window := self classToTest example] on: MessageNotUnderstood do: [  ] 
]

{ #category : #tests }
SpecTestCase >> testOpenWithSpec [
	self openInstance
]
