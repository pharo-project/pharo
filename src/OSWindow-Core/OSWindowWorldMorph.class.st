"
I am a morphic world that uses OSWindow.

For opening a World in a external window, the following script can be used:

world := OSWindowWorldMorph new.
world open.

"
Class {
	#name : #OSWindowWorldMorph,
	#superclass : #WorldMorph,
	#instVars : [
		'closeCallback',
		'isResizeable'
	],
	#category : #'OSWindow-Core-Morphic'
}

{ #category : #adding }
OSWindowWorldMorph >> addMorph: aMorph frame: rectangle [
	"For compatibility with SystemWindow."
	^ self addMorph: aMorph fullFrame: rectangle 


]

{ #category : #accessing }
OSWindowWorldMorph >> closeCallback [
	^ closeCallback
]

{ #category : #accessing }
OSWindowWorldMorph >> closeCallback: anObject [
	self flag: 'Do something better than just storing a closure.'.
	closeCallback := anObject
]

{ #category : #'submorphs-add/remove' }
OSWindowWorldMorph >> delete [
	closeCallback value.
	self class removeExtraWorld: self.
	worldState worldRenderer deactivate.
	super delete
]

{ #category : #initialization }
OSWindowWorldMorph >> initialize [
	super initialize.
	self color: Color veryLightGray.
	isResizeable := true.
	self extent: 640@480.
	self addHand: HandMorph new.
	
]

{ #category : #accessing }
OSWindowWorldMorph >> isResizeable [
	^ isResizeable
]

{ #category : #accessing }
OSWindowWorldMorph >> isResizeable: anObject [
	isResizeable := anObject
]

{ #category : #'instance creation' }
OSWindowWorldMorph >> open [
	self worldState worldRenderer: ((OSWorldRenderer forWorld: self)
		windowCloseAction: [self osWindowCloseButtonPressed]).
	self changed.
	self class addExtraWorld: self.
]

{ #category : #display }
OSWindowWorldMorph >> openInWorld [
	^ self open
]

{ #category : #events }
OSWindowWorldMorph >> osWindowCloseButtonPressed [
	self delete
]

{ #category : #accessing }
OSWindowWorldMorph >> title: aTitle [
	"self flag: 'ToDo'"
]
