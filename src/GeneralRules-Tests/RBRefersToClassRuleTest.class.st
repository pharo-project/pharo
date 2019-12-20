"
A RBRefersToClassRuleTest is a test class for testing the behavior of RBRefersToClassRule
"
Class {
	#name : #RBRefersToClassRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBRefersToClassRuleTest class >> isAbstract [
	"I can refer to RBRefersToClassRuleTest without error"
	'It is ok to refert to', RBRefersToClassRuleTest name.
	^ false 
]

{ #category : #'test-help' }
RBRefersToClassRuleTest >> referToClassName [
	"I refer to my own class by name - RBRefersToClassRuleTest"
	RBRefersToClassRuleTest name,
	88,
	RBRefersToClassRuleTest name
]

{ #category : #tests }
RBRefersToClassRuleTest >> testIsAbstractNotDetected [
	| critiques|
 	critiques := self myCritiguesOnMethod: self class class >> #isAbstract.

 	self assert: critiques size equals: 0.
]

{ #category : #tests }
RBRefersToClassRuleTest >> testRuleDetectHardCodedClass [
	| critiques|
 	critiques := self myCritiguesOnMethod: self class >> #referToClassName.

 	self assert: critiques size equals: 2.
 	self assert: (self sourceAtChritique:  critiques first) equals: 'RBRefersToClassRuleTest'.
]
