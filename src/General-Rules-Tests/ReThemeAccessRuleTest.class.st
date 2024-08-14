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

	| critiques model| 
	model := classfactory make: [ :builder | builder superclass: Model ].
	model compile: 'method ^ Smalltalk ui theme' classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: model >> #method.
	self assert: critiques size equals: 1
	
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleForMorph [

	| critiques model| 
	model := classfactory make: [ :builder | builder superclass: Morph ].
	model compile: 'method ^ Smalltalk ui theme' classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: model >> #method.
	self assert: critiques size equals: 1
	
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleNotViolatedForModel [ 

	| critiques model| 
	model := classfactory make: [ :builder | builder superclass: Model ].
	model compile: 'method ^ self theme' classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: model >> #method.
	self assertEmpty: critiques
	
]

{ #category : 'tests' }
ReThemeAccessRuleTest >> testRuleNotViolatedForMorph [

	| critiques model| 
	model := classfactory make: [ :builder | builder superclass: Morph ].
	model compile: 'method ^ self theme' classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: model >> #method.
	self assertEmpty: critiques
	
]
