"
A RBRefersToClassRuleTest is a test class for testing the behavior of RBRefersToClassRule
"
Class {
	#name : #ReRefersToClassRuleTest,
	#superclass : #ReAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
ReRefersToClassRuleTest class >> isAbstract [
	"I can refer to RBRefersToClassRuleTest without error"
	'It is ok to refert to', ReRefersToClassRuleTest name.
	^ false 
]

{ #category : #'test-help' }
ReRefersToClassRuleTest >> referToClassName [
	"I refer to my own class by name - RBRefersToClassRuleTest"
	ReRefersToClassRuleTest name,
	88,
	ReRefersToClassRuleTest name
]

{ #category : #tests }
ReRefersToClassRuleTest >> testIsAbstractNotDetected [
	| critiques|
 	critiques := self myCritiguesOnMethod: self class class >> #isAbstract.

 	self assert: critiques size equals: 0.
]

{ #category : #tests }
ReRefersToClassRuleTest >> testRuleDetectHardCodedClass [
	| critiques|
 	critiques := self myCritiguesOnMethod: self class >> #referToClassName.

 	self assert: critiques size equals: 2.
 	self assert: (self sourceAtChritique:  critiques first) equals: ReRefersToClassRuleTest name asString.
]
