"
A RBRefersToClassRuleTest is a test class for testing the behavior of RBRefersToClassRule
"
Class {
	#name : 'ReRefersToClassRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReRefersToClassRuleTest class >> isAbstract [
	"I can refer to RBRefersToClassRuleTest without error
	This method is present for the test of this class"
	'It is ok to refert to', ReRefersToClassRuleTest name.
	^ false
]

{ #category : 'sample' }
ReRefersToClassRuleTest >> referToClassName [
	"I refer to my own class by name - RBRefersToClassRuleTest"
	ReRefersToClassRuleTest name,
	88,
	ReRefersToClassRuleTest name
]

{ #category : 'tests' }
ReRefersToClassRuleTest >> testIsAbstractNotDetected [
	| critiques|
 	critiques := self myCritiquesOnMethod: self class class >> #isAbstract.

 	self assert: critiques size equals: 0
]

{ #category : 'tests' }
ReRefersToClassRuleTest >> testRuleDetectHardCodedClass [
	| critiques|
 	critiques := self myCritiquesOnMethod: self class >> #referToClassName.

 	self assert: critiques size equals: 2.
 	self assert: (self sourceAtChritique:  critiques first) equals: ReRefersToClassRuleTest name asString
]
