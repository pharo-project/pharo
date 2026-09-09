Class {
	#name : 'RBAbstractConditionTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'testing' }
RBAbstractConditionTest class >> isAbstract [

	^ self == RBAbstractConditionTest
]

{ #category : 'utilities' }
RBAbstractConditionTest >> modelOnClasses: aCollection [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: aCollection)
]
