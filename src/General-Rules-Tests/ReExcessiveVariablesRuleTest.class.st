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

	| critiques myClass |
	myClass := classFactory make: [ :builder |
		           builder slots:
			           { #a. #b. #c. #d. #e. #f. #g. #h. #i. #j. #k } ].
	critiques := self myCritiquesOnClass:  myClass.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReExcessiveVariablesRuleTest >> testRuleNotViolated [

	| critiques myClass |
	myClass := classFactory make: [ :builder |
		           builder slots: { #a. #b. #c } ].
	critiques := self myCritiquesOnClass: myClass.
	self assertEmpty: critiques
]
