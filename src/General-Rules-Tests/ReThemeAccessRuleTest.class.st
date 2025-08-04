Class {
	#name : 'ReThemeAccessRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classfactory'
	],
	#category : 'General-Rules-Tests-API Hints',
	#package : 'General-Rules-Tests',
	#tag : 'API Hints'
}

{ #category : 'running' }
ReThemeAccessRuleTest >> setUp [
	super setUp.
	classfactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
ReThemeAccessRuleTest >> tearDown [ 

	classfactory cleanUp.
	super tearDown 
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleForModel [

	| model |
	model := classfactory make: [ :builder | builder superclass: Model ].
	model compile: 'method ^ Smalltalk ui theme' classified: 'test-helper'.
	self assert: (self myCritiquesOnMethod: model >> #method) size equals: 1
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleForMorph [

	| model |
	model := classfactory make: [ :builder | builder superclass: Morph ].
	model compile: 'method ^ Smalltalk ui theme' classified: 'test-helper'.
	self assert: (self myCritiquesOnMethod: model >> #method) size equals: 1
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleNotViolatedForModel [

	| model |
	model := classfactory make: [ :builder | builder superclass: Model ].
	model compile: 'method ^ self theme' classified: 'test-helper'.
	self assertEmpty: (self myCritiquesOnMethod: model >> #method)
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleNotViolatedForMorph [

	| model |
	model := classfactory make: [ :builder | builder superclass: Morph ].
	model compile: 'method ^ self theme' classified: 'test-helper'.
	self assertEmpty: (self myCritiquesOnMethod: model >> #method)
]
