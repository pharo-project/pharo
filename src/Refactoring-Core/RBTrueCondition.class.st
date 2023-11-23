Class {
	#name : 'RBTrueCondition',
	#superclass : 'RBNewAbstractCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBTrueCondition >> check [

	^ true
]

{ #category : 'checking' }
RBTrueCondition >> errorString [ 

	^ ' true '
]

{ #category : 'checking' }
RBTrueCondition >> violators [ 

	^ OrderedCollection new
]
