Class {
	#name : 'ReNoUnusedInstanceVariableRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'running' }
ReNoUnusedInstanceVariableRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
ReNoUnusedInstanceVariableRuleTest >> tearDown [ 

	classFactory cleanUp.
	super tearDown 
]

{ #category : 'tests' }
ReNoUnusedInstanceVariableRuleTest >> testRule [ 

	| critiques class | 
	
	class := classFactory make: [ :builder | builder slots: { #inst } ].
	
	critiques := self myCritiquesOnClass: class.
	self assert: critiques size equals: 1
]
