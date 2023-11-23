"
A ReAbstractRuleTest is a test class for testing the behavior of ReAbstractRule
"
Class {
	#name : 'ReAbstractRuleTest',
	#superclass : 'TestCase',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'tests - testing' }
ReAbstractRuleTest >> testAbstractClassesShouldNotBeVisible [

	self assert: ReAbstractRule isAbstract. 
	self deny: ReAbstractRule isVisible 
]
