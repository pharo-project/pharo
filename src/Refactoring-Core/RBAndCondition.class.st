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

{ #category : 'private' }
RBAndCondition >> errorBlock [

	^ left check
		ifFalse: [ left errorBlock ]
		ifTrue: [ right errorBlock ]
]

{ #category : 'accessing' }
RBAndCondition >> errorString [ 
	
	left check ifFalse: [ ^ left errorString ].
	right check ifFalse: [ ^ right errorString ].
	^ ''
]

{ #category : 'initialization' }
RBAndCondition >> left: aCondition right: aCondition2 [
	left := aCondition.
	right := aCondition2.

]

{ #category : 'accessing' }
RBAndCondition >> nonViolators [ 
   ^ self check 
		ifTrue: [ left nonViolators , right nonViolators ]
		ifFalse: [ ^ #() ] 
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

	^ self check
		  ifFalse: [ left violators , right violators ]
		  ifTrue: [ #(  ) ]
]
