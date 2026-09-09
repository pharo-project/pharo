Class {
	#name : 'ReSpecShouldNotReferenceToolsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'running' }
ReSpecShouldNotReferenceToolsRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new
]

{ #category : 'running' }
ReSpecShouldNotReferenceToolsRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReSpecShouldNotReferenceToolsRuleTest >> testRule [

	| class |
	"Install the global so the following code compiles"
	classFactory environment at: #Smalltalk put: nil.
	class := classFactory make: [ :builder |
		         builder superclass: StPresenter ].

	class
		compile: 'method ^ Smalltalk tools inspector'
		classified: 'initialization'.

	self
		assert: (self myCritiquesOnMethod: class >> #method) size
		equals: 1
]

{ #category : 'tests' }
ReSpecShouldNotReferenceToolsRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ Smalltalk tools workspace open '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
