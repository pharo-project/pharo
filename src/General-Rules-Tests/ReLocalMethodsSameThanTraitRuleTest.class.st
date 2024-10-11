Class {
	#name : 'ReLocalMethodsSameThanTraitRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'running' }
ReLocalMethodsSameThanTraitRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
ReLocalMethodsSameThanTraitRuleTest >> tearDown [ 

	classFactory cleanUp.
	super tearDown 
]

{ #category : 'tests' }
ReLocalMethodsSameThanTraitRuleTest >> testRule [ 

	| critiques class trait|
	
	trait := classFactory newTrait.
	trait class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.
	
	class := classFactory make: [ :builder | builder traits: { trait } ].
	class class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.
	
	critiques := self myCritiquesOnClass: class class.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReLocalMethodsSameThanTraitRuleTest >> testRuleNotViolated [

	| critiques class trait|
	
	trait := classFactory newTrait.
	trait class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.
	
	class := classFactory make: [ :builder | builder traits: { trait } ].
	class class compile: 'method ^ ''I am leonTimo'' ' classified: 'test-helper'.
	
	critiques := self myCritiquesOnClass: class class.
	self assertEmpty: critiques 
]
