Class {
	#name : 'RePresenterShouldNotRedefineNewTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'running' }
RePresenterShouldNotRedefineNewTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
RePresenterShouldNotRedefineNewTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
RePresenterShouldNotRedefineNewTest >> testRule [

	| class |
	class := classFactory make: [ :builder | builder superclass: StPresenter ].

	class compile: 'new ^ 67' classified: 'initialization'.

	self assert: (self myCritiquesOnMethod: class >> #new) size equals: 1
]

{ #category : 'tests' }
RePresenterShouldNotRedefineNewTest >> testRuleNotViolated [

	| class |
	class := classFactory make: [ :builder | builder superclass: StPresenter ].

	class compile: 'method ^ 15' classified: 'initialization'.

	self assertEmpty: (self myCritiquesOnMethod: class >> #method)
]
