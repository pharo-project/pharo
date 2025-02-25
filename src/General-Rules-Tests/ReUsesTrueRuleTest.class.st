"
A RBUsesTrueRuleTest is a test class for testing the behavior of RBUsesTrueRule
"
Class {
	#name : 'ReUsesTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'sample' }
ReUsesTrueRuleTest >> sampleMethod [
	"You should not use True or False, but true and false. Unless it is the classes you refer to"
	| foo balse|
	foo := True.
	balse := False | foo
]

{ #category : 'tests' }
ReUsesTrueRuleTest >> testRule [
	| critiques|
 	critiques := self myCritiques .

 	self assert: critiques size equals: 2.
 	self assert: (self sourceAtChritique:  critiques second) equals: 'False'
]
