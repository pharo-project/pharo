Class {
	#name : 'RBSubtreeDoesNotContainReturnCondition',
	#superclass : 'RBSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBSubtreeDoesNotContainReturnCondition >> check [

	^ subtree containsReturn not
]

{ #category : 'displaying' }
RBSubtreeDoesNotContainReturnCondition >> violationMessageOn: aStream [ 

	aStream nextPutAll: 'Cannot extract selected code since it containt return.'
]
