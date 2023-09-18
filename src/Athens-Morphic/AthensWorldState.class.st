"
An AthensWorldState is a subclass of WorldState, that uses a special (fake) Display based on a real Morph. This is used to simulate a WorldMorph that actually resides in another Morph. This display adapts the owner dimensions as the ""DisplayScreen""-Dimension.


"
Class {
	#name : #AthensWorldState,
	#superclass : #WorldState,
	#instVars : [
		'display',
		'canvas'
	],
	#category : #'Athens-Morphic'
}

{ #category : #accessing }
AthensWorldState >> display [
	^ display
]

{ #category : #accessing }
AthensWorldState >> display: aDisplay [
	display := aDisplay
]

{ #category : #'update cycle' }
AthensWorldState >> doDeferredUpdatingFor: aWorld [
	"overwritten from WorldState to use ower own display"

	(self display deferUpdates: true) ifNil: [ ^ false ].
	(canvas notNil and: [ canvas form == Display ])
		ifFalse: [
			aWorld viewBox: self display boundingBox.	"do first since it may clear canvas"
			self canvas: (self display getCanvas copyClipRect: self display boundingBox) ].
	^ true
]
