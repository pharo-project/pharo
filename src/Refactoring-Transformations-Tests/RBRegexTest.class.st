Class {
	#name : 'RBRegexTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Transformations-Tests',
	#package : 'Refactoring-Transformations-Tests'
}

{ #category : 'tests' }
RBRegexTest >> testRegexWithNoStartingAcronym [
	
	| refact |
	refact := RBRegexRefactoring new replace: '^(.*)$' with: 'RB','$1'. 
	self assert: (refact execute: 'YKFoo') equals: 'RBYKFoo'

]

{ #category : 'tests' }
RBRegexTest >> testRegexWithTwoAcronyms [
	
	| refact |
	refact := RBRegexRefactoring new replace: '^YK(.*)$' with: 'RB','$1'. 
	self assert: (refact execute: 'YKFoo') equals: 'RBFoo'

]
