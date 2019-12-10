"
A RBMethodSourceContainsLinefeedsRuleTest is a test class for testing the behavior of RBMethodSourceContainsLinefeedsRule
"
Class {
	#name : #RBMethodSourceContainsLinefeedsRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBMethodSourceContainsLinefeedsRuleTest >> foo [
	"This method contains two linefeeds which should be carriage returns."
	"There is one after this line" 7.
	"Noone here" 9.
	"and one here" 8 .
	"That was it." -1
]

{ #category : #tests }
RBMethodSourceContainsLinefeedsRuleTest >> testRule [
	| fooMethod critiques|
	fooMethod := 'foo
	"This method contains two linefeeds which should be carriage returns."
	"There is one after this line" 7.
	"Noone here" 9.
	"and one here" 8 .
	"That was it." -1'.
	self class compile: fooMethod classified: 'test-help'.
 	critiques := self myCritiquesOnFoo .

 	self assert: critiques size equals: 2.
 	self assert: critiques first sourceAnchor interval equals: (111 to: 111).
]
