Class {
	#name : 'ReGenerateEqualAndHashDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> classForEqualAndHash [
	"Answer the <Class> which will be used to generate equal and hash methods"
	
	^ RBClassForGeneratingEqualAndHash 
]

{ #category : 'running' }
ReGenerateEqualAndHashDriverTest >> setUp [

	super setUp.
	testingEnvironment := RBClassEnvironment class: self classForEqualAndHash. 
]

{ #category : 'running' }
ReGenerateEqualAndHashDriverTest >> tearDown [

	testingEnvironment ifNotNil: [ 
		testingEnvironment classesDo: [ : cls |
			(cls includesSelector: #=)
				ifTrue: [ cls removeSelector: #= ].
			(cls includesSelector: #hash)
				ifTrue: [ cls removeSelector: #hash ] ] ].
	super tearDown.
]

{ #category : 'tests' }
ReGenerateEqualAndHashDriverTest >> testGenerateEqualAndHash [

	| driver rbClass driverChanges |

	^ self skip.
	driver := ReGenerateEqualAndHashDriver basicNew. 
	self setUpDriver: driver.
	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment class.

	self deny: (driver targetClass definesMethod: #hash).
	driver runRefactoring.
	driverChanges := driver refactoring changes.
	self 
		assert: driverChanges changes size 
		equals: 4.
	self assert: (driver targetClass definesMethod: #hash).
	self assert: (driver targetClass definesMethod: #=).
]
