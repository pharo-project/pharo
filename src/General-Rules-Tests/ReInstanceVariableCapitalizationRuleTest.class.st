Class {
	#name : 'ReInstanceVariableCapitalizationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'running' }
ReInstanceVariableCapitalizationRuleTest >> setUp [
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReInstanceVariableCapitalizationRuleTest >> tearDown [ 

	classFactory cleanUp.
	super tearDown 
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleForClassSide [

	| class |
	class := classFactory make: [ :aBuilder | aBuilder classSlots: { #Instance } ].
	self assert: (self myCritiquesOnClass: class class) size equals: 1
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleForInstanceSide [

	| class |
	class := classFactory make: [ :aBuilder | aBuilder slots: { #Instance } ].
	self assert: (self myCritiquesOnClass: class) size equals: 1
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleNotViolatedForClassSide [

	| class |
	class := classFactory make: [ :aBuilder | aBuilder classSlots: { #instance } ].
	self assertEmpty: (self myCritiquesOnClass: class class)
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleNotViolatedForInstanceSide [

	| class |
	class := classFactory make: [ :aBuilder | aBuilder slots: { #instance } ].
	self assertEmpty: (self myCritiquesOnClass: class)
]
