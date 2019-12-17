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

{ #category : #'test-help' }
RBTempVarOverridesInstVarRuleTest >> sampleMethod: dummy1 [
	"I override dummy1, dummy2, dummy3  "
	| dummy2 |
	dummy2 := [ :dummy3 | dummy4 := dummy1 + dummy3 ].
	^ dummy2 value: dummy1.
	
]

{ #category : #tests }
RBTempVarOverridesInstVarRuleTest >> testRule [
	| critiques |
	self class compile: 'sampleMethod: dummy1
	"I override dummy1, dummy2, dummy3  "
	| dummy2 |
	dummy2 := [ :dummy3 | dummy4 := dummy1 + dummy3 ].
	^ dummy2 value: dummy1.
	' classified: 'test-help'.
	critiques := self myCritiguesOnMethod: self class >> #sampleMethod:.
	
	self assert: critiques size equals: 2.
	self assert: (self sourceAtChritique:  critiques second) equals: 'dummy2'.
]
