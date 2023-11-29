"
SUnit tests for Gofer operations
"
Class {
	#name : 'GoferOperationTest',
	#superclass : 'GoferTest',
	#instVars : [
		'environment'
	],
	#category : 'Gofer-Tests-Tests',
	#package : 'Gofer-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
GoferOperationTest >> environment [
	^ environment
]

{ #category : 'accessing' }
GoferOperationTest >> environment: anObject [

	environment := anObject
]

{ #category : 'utilities' }
GoferOperationTest >> hasClass: aSymbol [

	^ environment includesKey: aSymbol
]

{ #category : 'utilities' }
GoferOperationTest >> hasClass: aSymbol selector: aSelector [

	^ (environment classNamed: aSymbol) includesSelector: aSelector
]

{ #category : 'utilities' }
GoferOperationTest >> hasPackage: aString [

	^ MCWorkingCopy allWorkingCopies anySatisfy: [ :package | package packageName = aString ]
]

{ #category : 'utilities' }
GoferOperationTest >> hasVersion: aString [

	^ MCWorkingCopy allWorkingCopies anySatisfy: [ :version | version ancestry ancestorString = aString ]
]

{ #category : 'running' }
GoferOperationTest >> setUp [

	super setUp.
	self setUpEnvironment.
	gofer repository: self monticelloRepository
]

{ #category : 'running' }
GoferOperationTest >> setUpEnvironment [

	environment := self class environment
]

{ #category : 'running' }
GoferOperationTest >> tearDown [

	(self hasPackage: 'GoferFoo')
		ifTrue: [ Gofer new package: 'GoferFoo'; unload ].
	(self hasPackage: 'GoferBar')
		ifTrue: [ Gofer new package: 'GoferBar'; unload ].
	(self hasPackage: 'ConfigurationOfGoferFoo')
		ifTrue: [ Gofer new package: 'ConfigurationOfGoferFoo'; unload ].
	super tearDown
]

{ #category : 'tests' }
GoferOperationTest >> testLoad [

	gofer version: 'GoferFoo-lr.1'.
	gofer load.
	self assert: (self hasVersion: 'GoferFoo-lr.1').
	self assert: (self hasClass: #GoferFoo)
]
