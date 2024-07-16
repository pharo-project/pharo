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


	| class critiques | 
	class := classFactory make: [ :aBuilder | aBuilder  classSlots: { #Instance } ].
	critiques := self myCritiquesOnClass: class class.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleForInstanceSide [


	| class critiques | 
	class := classFactory make: [ :aBuilder | aBuilder slots: { #Instance } ].
	critiques := self myCritiquesOnClass: class.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleNotViolatedForClassSide [


	| class critiques | 
	class := classFactory make: [ :aBuilder | aBuilder  classSlots: { #instance } ].
	critiques := self myCritiquesOnClass: class class.
	self assertEmpty: critiques 
]

{ #category : 'tests' }
ReInstanceVariableCapitalizationRuleTest >> testRuleNotViolatedForInstanceSide [


	| class critiques | 
	class := classFactory make: [ :aBuilder | aBuilder  slots: { #instance } ].
	critiques := self myCritiquesOnClass: class.
	self assertEmpty: critiques 
]
