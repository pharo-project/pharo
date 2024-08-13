"
A RBMethodSourceContainsLinefeedsRuleTest is a test class for testing the behavior of RBMethodSourceContainsLinefeedsRule
"
Class {
	#name : 'ReMethodSourceContainsLinefeedsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'test-help' }
ReMethodSourceContainsLinefeedsRuleTest >> sampleMethod [
	"This method contains two linefeeds which should be carriage returns."
	"There is one after this line" 7.
	"Noone here" 9.
	"and one here" 8 .
	"That was it." -1
]

{ #category : 'tests' }
ReMethodSourceContainsLinefeedsRuleTest >> testRule [
	| sampleMethod critiques|
	sampleMethod := 'sampleMethod
	"This method contains two linefeeds which should be carriage returns."
	"There is one after this line" 7.', String lf,
	'	"Noone here" 9.
	"and one here" 8 .', String lf,
	'	"That was it." -1'.
	self class compile: sampleMethod classified: 'test-help'.
 	critiques := self myCritiques .

 	self assert: critiques size equals: 2.
 	self assert: (self sourceAtChritique:  critiques first) equals: String lf
]
