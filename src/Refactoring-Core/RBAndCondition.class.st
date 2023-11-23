Class {
	#name : 'RBAndCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'left',
		'right'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBAndCondition >> check [
	left check
		ifFalse:
			[ ^ false].
	right check
		ifFalse:
			[ ^ false].
	^true
]

{ #category : 'accessing' }
RBAndCondition >> errorString [ 
	"I doubt because I thought it was lazy"
	
	^ right errorString, ' and ', left errorString
]

{ #category : 'initialization' }
RBAndCondition >> left: aCondition right: aCondition2 [
	left := aCondition.
	right := aCondition2.

]

{ #category : 'printing' }
RBAndCondition >> printOn: aStream [
	aStream
		print: left;
		nextPutAll: ' & ';
		print: right
]

{ #category : 'accessing' }
RBAndCondition >> violators [ 

	^ left violators , right violators
]
