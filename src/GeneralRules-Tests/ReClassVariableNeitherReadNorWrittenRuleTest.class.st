Class {
	#name : 'ReClassVariableNeitherReadNorWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'GeneralRules-Tests-Migrated',
	#package : 'GeneralRules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
ReClassVariableNeitherReadNorWrittenRuleTest >> testRule [
	| critiques |
	critiques := self myCritiquesOnClass: RBLintRuleTestData.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReClassVariableNeitherReadNorWrittenRuleTest >> testRuleDoesNotAppear [
	| critiques |
	critiques := self myCritiquesOnClass: self class.
	self assert: critiques size equals: 0
]
