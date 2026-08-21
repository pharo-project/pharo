Class {
	#name : 'RBTrueCondition',
	#superclass : 'RBNewAbstractCondition',
	#category : 'Refactoring-Core-OldConditions',
	#package : 'Refactoring-Core',
	#tag : 'OldConditions'
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
