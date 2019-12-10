"
A RBUncommonMessageSendRuleTest is a test class for testing the behavior of RBUncommonMessageSendRule
"
Class {
	#name : #RBUncommonMessageSendRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBUncommonMessageSendRuleTest >> foo [
	"I check that no uncommon method sends occour. For instance using self or true as method names, or method names with capital - like '7 BadSelector'"
	99 self.
	3 factorial.
	7 BadSelector.
]

{ #category : #tests }
RBUncommonMessageSendRuleTest >> testCheckForCritiques [
	| critiques |
	critiques := self myCritiquesOnFoo.
	self assert: critiques size equals: 2.
	self
		assert: critiques first sourceAnchor interval
		equals: (156 to: 162)
]
