Class {
	#name : #HERingBinder,
	#superclass : #Object,
	#instVars : [
		'ringClass',
		'ringMethod'
	],
	#category : 'Hermes-Ring'
}

{ #category : #'instance creation' }
HERingBinder class >> onMethod: aRFiMethod [
	^ self new
		onMethod: aRFiMethod;
		yourself
]

{ #category : #accessing }
HERingBinder >> allSlots [
	^ ringClass allSlots
]

{ #category : #accessing }
HERingBinder >> binding [
	" The actual value is never persisted, but we are returing just in case Opal."
	^ nil -> ringClass
]

{ #category : #initialization }
HERingBinder >> bindingOf: aName [
"	(ringMethod ast hasTemporaryNamed: aName)
		ifTrue: [ ^ nil ].

	(ringMethod ast hasArgumentNamed: aName)
		ifTrue: [ ^ nil ].

	"
	^ (ringClass bindingOf: aName)"
	binding isNil] whileTrue: [
		 ]

	ringClass environment at: aName ifPresent: [ :aClass | ^ GlobalVariable key: aName value: aClass ] ifAbsent: [  ].

	self halt"
]

{ #category : #initialization }
HERingBinder >> onMethod: aRFiMethod [ 
	ringClass := aRFiMethod methodClass.
	ringMethod := aRFiMethod 
]
