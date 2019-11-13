"
I am a Mock to test RBRefersToClassRule.
"
Class {
	#name : #RBAbstractMockToTestRefersToClassRule,
	#superclass : #Object,
	#category : #'GeneralRules-Tests-Mock'
}

{ #category : #testing }
RBAbstractMockToTestRefersToClassRule class >> isAbstract [
	^ self = RBAbstractMockToTestRefersToClassRule
]

{ #category : #helpers }
RBAbstractMockToTestRefersToClassRule >> methodHardCodingClass [
	^ RBAbstractMockToTestRefersToClassRule
]

{ #category : #helpers }
RBAbstractMockToTestRefersToClassRule >> methodWithoutCodingClass [
	^ 42
]
