"
A ReBaselineProperlyPackagedRuleTest is a test class for testing the behavior of ReBaselineProperlyPackagedRule
"
Class {
	#name : 'ReBaselineProperlyPackagedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'running' }
ReBaselineProperlyPackagedRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReBaselineProperlyPackagedRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReBaselineProperlyPackagedRuleTest >> testRuleWithBadPackagedBaselineClass [

	| badPackagedBaselineClass critiques |
	badPackagedBaselineClass := classFactory make: [ :builder |
		                    builder
			                    name: 'BaselineOfSimple';
			                    tag: 'Base';
			                    package: 'Simple' ].

	critiques := self myCritiquesOnClass: badPackagedBaselineClass.
	self assert: critiques size equals: 1.

	"Check it does not lament on class side"
	critiques := self myCritiquesOnClass: badPackagedBaselineClass class.
	self assert: critiques isEmpty
]

{ #category : 'tests' }
ReBaselineProperlyPackagedRuleTest >> testRuleWithWellPackagedBaselineClass [

	| wellPackagedBaselineClass critiques |
	wellPackagedBaselineClass := classFactory make: [ :builder |
		                    builder
			                    name: 'BaselineOfSimple';
			                    tag: 'Base';
			                    package: 'BaselineOfSimple' ].

	critiques := self myCritiquesOnClass: wellPackagedBaselineClass.
	self assert: critiques isEmpty.

	"Check it does not lament on class side"
	critiques := self myCritiquesOnClass: wellPackagedBaselineClass class.
	self assert: critiques isEmpty
]
