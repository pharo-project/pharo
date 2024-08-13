"
A ReBaselineProperlyPackagedRuleTest is a test class for testing the behavior of ReBaselineProperlyPackagedRule
"
Class {
	#name : 'ReBaselineWithProperSuperclassRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'running' }
ReBaselineWithProperSuperclassRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReBaselineWithProperSuperclassRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReBaselineWithProperSuperclassRuleTest >> testRuleWithBaselineClassWithCorrectSuperclass [

	| badPackagedBaselineClass critiques |
	badPackagedBaselineClass := classFactory make: [ :builder |
		                    builder
			                    name: 'BaselineOfSimple';
									  superclass: BaselineOf;
			                    tag: 'Base';
			                    package: 'Simple' ].

	critiques := self myCritiquesOnClass: badPackagedBaselineClass.
	self assert: critiques isEmpty.

	"Check it does not lament on class side"
	critiques := self myCritiquesOnClass: badPackagedBaselineClass class.
	self assert: critiques isEmpty
]

{ #category : 'tests' }
ReBaselineWithProperSuperclassRuleTest >> testRuleWithBaselineClassWithWrongSuperclass [

	| badPackagedBaselineClass critiques |
	badPackagedBaselineClass := classFactory make: [ :builder |
		                    builder
			                    name: 'BaselineOfSimple';
									  superclass: Object;
			                    tag: 'Base';
			                    package: 'Simple' ].

	critiques := self myCritiquesOnClass: badPackagedBaselineClass.
	self assert: critiques size equals: 1.

	"Check it does not lament on class side"
	critiques := self myCritiquesOnClass: badPackagedBaselineClass class.
	self assert: critiques isEmpty
]
