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

	| class |
	class := classfactory make: [ :builder | builder slots: { #inst } ].
	class compile: 'initialize super initialize. inst := nil' classified: 'initialization'.
	self assert: (self myCritiquesOnMethod: class >> #initialize) size equals: 1
]

{ #category : 'tests' }
ReNoNilAssignationInInitializeRuleTest >> testRuleNotViolated [

	| class |
	class := classfactory make: [ :builder | builder slots: { #inst } ].
	class compile: 'initialize super initialize' classified: 'initialization'.
	self assertEmpty: (self myCritiquesOnMethod: class >> #initialize)
]
