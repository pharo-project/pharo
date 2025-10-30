"
I succeed when a message send is not inside a cascade node.
"
Class {
	#name : 'ReNotInCascadedMessageCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReNotInCascadedMessageCondition >> candidates [

	^ { subtree }
]

{ #category : 'checking' }
ReNotInCascadedMessageCondition >> check [

	subtree ifNil: [ ^ false ].
	^ subtree parent isCascade not
]

{ #category : 'displaying' }
ReNotInCascadedMessageCondition >> violationMessageOn: aStream [ 

	^ aStream nextPutAll: 'Cannot extract code in a cascaded message.'
]

{ #category : 'displaying' }
ReNotInCascadedMessageCondition >> violationMessageOn: aStream violators: theViolators [

	theViolators = self violators 
		ifTrue: [ aStream nextPutAll: 'Cannot extract code in a cascaded message.' ] 
		ifFalse: [ aStream nextPutAll: 'Can extract code in a non cascaded message.' ]
]

{ #category : 'accessing' }
ReNotInCascadedMessageCondition >> violators [

	subtree ifNil: [ ^ #(  ) ].
	^ subtree parent isCascade
		  ifTrue: [ { subtree } ]
		  ifFalse: [ #(  ) ]
]
