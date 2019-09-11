"
i holding a collection of various key/button modifier flags, usually carried by certain events and accessible through #modifiers message.
"
Class {
	#name : #OSStateModifiers,
	#superclass : #Object,
	#instVars : [
		'leftShift',
		'rightShift',
		'leftAlt',
		'rightAlt',
		'leftCtrl',
		'rightCtrl',
		'leftCmd',
		'rightCmd',
		'capsLock',
		'numLock',
		'buttons'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #accessing }
OSStateModifiers >> alt [
	^ leftAlt or: [  rightAlt ]
]

{ #category : #accessing }
OSStateModifiers >> any [ 

	^ self cmd | self alt | self ctrl
]

{ #category : #accessing }
OSStateModifiers >> buttons [
	^ buttons
]

{ #category : #accessing }
OSStateModifiers >> capsLock [
	
	^ capsLock
]

{ #category : #accessing }
OSStateModifiers >> capsLock: anObject [
	
	capsLock := anObject
]

{ #category : #accessing }
OSStateModifiers >> cmd [
	^ leftCmd or: [  rightCmd ]
]

{ #category : #accessing }
OSStateModifiers >> ctrl [
	^ leftCtrl or: [  rightCtrl ]
]

{ #category : #initialization }
OSStateModifiers >> initialize [
	leftAlt := leftCmd := leftCtrl := leftShift := rightAlt := rightCmd := rightCtrl := rightShift :=
	capsLock := numLock := false.
	buttons := OSButtonState new.
]

{ #category : #accessing }
OSStateModifiers >> leftAlt [
	
	^ leftAlt
]

{ #category : #accessing }
OSStateModifiers >> leftAlt: anObject [
	
	leftAlt := anObject
]

{ #category : #accessing }
OSStateModifiers >> leftCmd [
	
	^ leftCmd
]

{ #category : #accessing }
OSStateModifiers >> leftCmd: anObject [
	
	leftCmd := anObject
]

{ #category : #accessing }
OSStateModifiers >> leftCtrl [
	
	^ leftCtrl
]

{ #category : #accessing }
OSStateModifiers >> leftCtrl: anObject [
	
	leftCtrl := anObject
]

{ #category : #accessing }
OSStateModifiers >> leftShift [
	
	^ leftShift
]

{ #category : #accessing }
OSStateModifiers >> leftShift: anObject [
	
	leftShift := anObject
]

{ #category : #accessing }
OSStateModifiers >> numLock [
	
	^ numLock
]

{ #category : #accessing }
OSStateModifiers >> numLock: anObject [
	
	numLock := anObject
]

{ #category : #printing }
OSStateModifiers >> printOn: aStream [
	aStream nextPutAll: self class name; space; nextPut: $[.
	leftAlt ifTrue: [ aStream nextPutAll: 'leftAlt'; space ].
	leftCmd ifTrue: [ aStream nextPutAll: 'leftCmd'; space ].
	leftCtrl ifTrue: [ aStream nextPutAll: 'leftCtrl'; space ].
	leftShift ifTrue: [ aStream nextPutAll: 'leftShift'; space ].
	rightAlt ifTrue: [ aStream nextPutAll: 'rightAlt'; space ].
	rightCmd ifTrue: [ aStream nextPutAll: 'rightCmd'; space ].
	rightCtrl ifTrue: [ aStream nextPutAll: 'rightCtrl'; space ].
	rightShift ifTrue: [ aStream nextPutAll: 'rightShift'; space ].
	capsLock ifTrue: [ aStream nextPutAll: 'capsLock'; space ].
	numLock ifTrue: [ aStream nextPutAll: 'numLock'; space ].
	
	aStream nextPut: $]; space; print: buttons
]

{ #category : #accessing }
OSStateModifiers >> rightAlt [
	
	^ rightAlt
]

{ #category : #accessing }
OSStateModifiers >> rightAlt: anObject [
	
	rightAlt := anObject
]

{ #category : #accessing }
OSStateModifiers >> rightCmd [
	
	^ rightCmd
]

{ #category : #accessing }
OSStateModifiers >> rightCmd: anObject [
	
	rightCmd := anObject
]

{ #category : #accessing }
OSStateModifiers >> rightCtrl [
	
	^ rightCtrl
]

{ #category : #accessing }
OSStateModifiers >> rightCtrl: anObject [
	
	rightCtrl := anObject
]

{ #category : #accessing }
OSStateModifiers >> rightShift [
	
	^ rightShift
]

{ #category : #accessing }
OSStateModifiers >> rightShift: anObject [
	
	rightShift := anObject
]

{ #category : #accessing }
OSStateModifiers >> shift [
	^ leftShift or: [  rightShift ]
]
