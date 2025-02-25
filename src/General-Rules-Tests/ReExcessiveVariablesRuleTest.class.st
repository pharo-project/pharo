Class {
	#name : 'ReExcessiveVariablesRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'running' }
ReExcessiveVariablesRuleTest >> setUp [ 
	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReExcessiveVariablesRuleTest >> tearDown [ 
	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReExcessiveVariablesRuleTest >> testRule [

	| myClass |
	myClass := classFactory make: [ :builder | builder slots: { #a. #b. #c. #d. #e. #f. #g. #h. #i. #j. #k } ].
	self assert: (self myCritiquesOnClass: myClass) size equals: 1
]

{ #category : 'tests' }
ReExcessiveVariablesRuleTest >> testRuleNotViolated [

	| myClass |
	myClass := classFactory make: [ :builder | builder slots: { #a. #b. #c } ].
	self assertEmpty: (self myCritiquesOnClass: myClass)
]
