Class {
	#name : 'RBTrueCondition',
	#superclass : 'RBCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBTrueCondition >> check [

	^ true
]

{ #category : 'checking' }
RBTrueCondition >> violators [ 

	^ #()
]
