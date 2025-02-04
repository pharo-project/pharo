Class {
	#name : 'ReNoNilAssignationInInitializeRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classfactory'
	],
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'running' }
ReNoNilAssignationInInitializeRuleTest >> setUp [
	super setUp.
	classfactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
ReNoNilAssignationInInitializeRuleTest >> tearDown [ 

	classfactory cleanUp.
	super tearDown 
	
]

{ #category : 'tests' }
ReNoNilAssignationInInitializeRuleTest >> testRule [ 

	| critiques class |
	class := classfactory make: [ :builder | builder slots: { #inst } ].
	class compile: 'initialize super initialize. inst := nil' classified: 'initialization' .
	
	critiques := self myCritiquesOnMethod: class >> #initialize.
	self assert: critiques size equals: 1
	
]

{ #category : 'tests' }
ReNoNilAssignationInInitializeRuleTest >> testRuleNotViolated [ 

	| critiques class |
	class := classfactory make: [ :builder | builder slots: { #inst } ].
	class compile: 'initialize super initialize' classified: 'initialization' .
	
	critiques := self myCritiquesOnMethod: class >> #initialize.
	self assertEmpty: critiques
	
]
