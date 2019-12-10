"
A RBUsesTrueRuleTest is a test class for testing the behavior of RBUsesTrueRule
"
Class {
	#name : #RBUsesTrueRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBUsesTrueRuleTest >> foo [
	"You should not use True or False, but true and false. Unless it is the classes you refer to"
	| foo balse|
	foo := True.
	balse := False | foo
]

{ #category : #'test-help' }
RBUsesTrueRuleTest >> testCheckForCritiques [
	| critiques|
 	critiques := self myCritiquesOnFoo .

 	self assert: critiques size equals: 2.
 	self assert: critiques first sourceAnchor interval equals: (122 to: 125).
]
