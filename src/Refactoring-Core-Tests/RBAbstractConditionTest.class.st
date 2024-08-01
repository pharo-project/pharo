Class {
	#name : 'RBAbstractConditionTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
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
