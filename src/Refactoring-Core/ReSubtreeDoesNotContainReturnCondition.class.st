Class {
	#name : 'ReSubtreeDoesNotContainReturnCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSubtreeDoesNotContainReturnCondition >> check [

	^ subtree containsReturn not
]

{ #category : 'displaying' }
ReSubtreeDoesNotContainReturnCondition >> violationMessageOn: aStream [ 

	aStream nextPutAll: 'Cannot extract selected code since it containt return.'
]
