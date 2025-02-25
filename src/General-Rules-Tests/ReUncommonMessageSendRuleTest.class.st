"
A RBUncommonMessageSendRuleTest is a test class for testing the behavior of RBUncommonMessageSendRule
"
Class {
	#name : 'ReUncommonMessageSendRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'sample' }
ReUncommonMessageSendRuleTest >> sampleMethod [
	"I check that no uncommon method sends occour. For instance using self or true as method names, or method names with capital - like '7 BadSelector'"
	<ignoreNotImplementedSelectors: #(self BadSelector)>
	99 self.
	3 factorial.
	7 BadSelector
]

{ #category : 'tests' }
ReUncommonMessageSendRuleTest >> testRule [
	| critiques |
	critiques := self myCritiques.

	self assert: critiques size equals: 2.
	self assert: (self sourceAtChritique:  critiques second) equals: '7 BadSelector'
]
