"
A ReMethodSelectorKeywordCasingRuleTest is a test class for testing the behavior of ReMethodSelectorKeywordCasingRule
"
Class {
	#name : 'ReMethodSelectorKeywordCasingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests - test data' }
ReMethodSelectorKeywordCasingRuleTest >> KeyWordMethodWith: a wrongCasingAtEnd: b [
	"This is a method used to provoke a lint for testing"
]

{ #category : 'tests - test data' }
ReMethodSelectorKeywordCasingRuleTest >> keyWordMethodWith: a WrongCasingAtEnd: b [
	"This is a method used to provoke a lint for testing"
]

{ #category : 'tests' }
ReMethodSelectorKeywordCasingRuleTest >> testRule [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #keyWordMethodWith:WrongCasingAtEnd:.

	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReMethodSelectorKeywordCasingRuleTest >> testRuleUppercaseAtStart [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #KeyWordMethodWith:wrongCasingAtEnd:.

	self assert: critiques size equals: 1
]
