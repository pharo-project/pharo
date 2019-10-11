"
A RBRefersToClassRuleTest is a test class for testing the behavior of RBRefersToClassRule
"
Class {
	#name : #RBRefersToClassRuleTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #helpers }
RBRefersToClassRuleTest >> newRule [
	^ RBRefersToClassRule new
]

{ #category : #tests }
RBRefersToClassRuleTest >> testIsAbstractNotDetected [
	self assertEmpty: (self newRule check: RBAbstractMockToTestRefersToClassRule class>> #isAbstract)
]

{ #category : #tests }
RBRefersToClassRuleTest >> testRuleDetectHardCodedClass [
	self denyEmpty: (self newRule check: RBAbstractMockToTestRefersToClassRule >> #methodHardCodingClass)
]

{ #category : #tests }
RBRefersToClassRuleTest >> testRuleWithoutHardCodedClass [
	self assertEmpty: (self newRule check: RBAbstractMockToTestRefersToClassRule >> #methodWithoutCodingClass)
]
