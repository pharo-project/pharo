Class {
	#name : 'ReClassVariableNeitherReadNorWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Clean Code',
	#package : 'General-Rules-Tests',
	#tag : 'Clean Code'
}

{ #category : 'tests' }
ReClassVariableNeitherReadNorWrittenRuleTest >> testRule [

	self denyEmpty: (self subjectUnderTest new unreferencedVariablesOf: RBLintRuleTestData)
]

{ #category : 'tests' }
ReClassVariableNeitherReadNorWrittenRuleTest >> testRuleDoesNotAppear [
	| critiques |
	critiques := self myCritiquesOnClass: self class.
	self assert: critiques size equals: 0
]
