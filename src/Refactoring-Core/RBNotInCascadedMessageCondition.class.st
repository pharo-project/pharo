Class {
	#name : 'RBNotInCascadedMessageCondition',
	#superclass : 'RBSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBNotInCascadedMessageCondition >> check [

	^ subtree parent isCascade not
]

{ #category : 'displaying' }
RBNotInCascadedMessageCondition >> violationMessageOn: aStream [ 

	^ aStream nextPutAll: 'Cannot extract code in a cascaded message.'
]
