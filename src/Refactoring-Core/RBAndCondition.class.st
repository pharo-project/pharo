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

{ #category : 'accessing - private' }
RBAndCondition >> candidates [ 

	^ left candidates , right candidates
]

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

{ #category : 'accessing' }
RBAndCondition >> nonViolators [ 
	(left nonViolators isEmpty and: [ right nonViolators isEmpty ])
		ifTrue: [ ^ #() ].
	^ left nonViolators , right nonViolators 
]

{ #category : 'printing' }
RBAndCondition >> printOn: aStream [
	aStream
		print: left;
		nextPutAll: ' & ';
		print: right
]

{ #category : 'displaying' }
RBAndCondition >> violationMessageOn: aStream violators: theViolators [
	| violatorsLeft violatorsRight |
	self flag: 'This implementation wont work for conditions whose candidates cannot be compared by sending =. Fix me!'.
	violatorsLeft := left candidates select: [ :each | theViolators includes: each ].
	violatorsRight := right candidates select: [ :each | theViolators includes: each ].
	
	left violationMessageOn: aStream violators: violatorsLeft.
	aStream nextPutAll: ' & '.
	right violationMessageOn: aStream violators: violatorsRight
]

{ #category : 'accessing' }
RBAndCondition >> violators [ 

	^ left violators , right violators
]
