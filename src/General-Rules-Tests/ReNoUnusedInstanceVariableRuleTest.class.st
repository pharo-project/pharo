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

	| class |
	class := classFactory make: [ :builder | builder slots: { #inst } ].
	self assert: (self myCritiquesOnClass: class) size equals: 1
]
