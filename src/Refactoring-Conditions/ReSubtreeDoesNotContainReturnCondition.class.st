Class {
	#name : 'ReSubtreeDoesNotContainReturnCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReSubtreeDoesNotContainReturnCondition >> check [

	^ subtree containsReturn not
]

{ #category : 'displaying' }
ReSubtreeDoesNotContainReturnCondition >> violationMessageOn: aStream [ 

	aStream nextPutAll: 'Cannot extract selected code since it containt return.'
]
