"
A RBTempVarOverridesInstVarRuleTest is a test class for testing the behavior of RBTempVarOverridesInstVarRule
"
Class {
	#name : #RBTempVarOverridesInstVarRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#instVars : [
		'dummy1',
		'dummy2',
		'dummy3',
		'dummy4'
	],
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'text-help' }
RBTempVarOverridesInstVarRuleTest >> foo: dummy1 [
	"I override dummy1, dummy2, dummy3  "
	| dummy2 |
	dummy2 := [ :dummy3 | dummy4 := dummy1 + dummy3 ].
	^ dummy2 value: dummy1.
	
]

{ #category : #tests }
RBTempVarOverridesInstVarRuleTest >> testRule [
	| critiques |
	critiques := self myCritiguesOnMethod: self class >> #foo:.
	self assert: critiques size equals: 2.
	self assert: critiques second sourceAnchor interval equals: (55 to: 60)
]
