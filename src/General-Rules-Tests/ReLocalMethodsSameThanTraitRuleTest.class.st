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

	| class trait |
	trait := classFactory newTrait.
	trait class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.

	class := classFactory make: [ :builder | builder traits: { trait } ].
	class class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.
	self assert: (self myCritiquesOnClass: class class) size equals: 1
]

{ #category : 'tests' }
ReLocalMethodsSameThanTraitRuleTest >> testRuleNotViolated [

	| class trait |
	trait := classFactory newTrait.
	trait class compile: 'method ^ ''I am Timoleon'' ' classified: 'test-helper'.

	class := classFactory make: [ :builder | builder traits: { trait } ].
	class class compile: 'method ^ ''I am leonTimo'' ' classified: 'test-helper'.
	self assertEmpty: (self myCritiquesOnClass: class class)
]
