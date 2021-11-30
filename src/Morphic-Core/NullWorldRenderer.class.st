"
I am a null renderer.
I am used when we are in command line or during the initialization
"
Class {
	#name : #NullWorldRenderer,
	#superclass : #AbstractWorldRenderer,
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
NullWorldRenderer class >> isApplicableFor: aWorld [
	"This worldRenderer is always applicable"

	^ true
]

{ #category : #operations }
NullWorldRenderer >> activateCursor: aCursor withMask: maskForm [
	"Nothing to activate here"
]

{ #category : #accessing }
NullWorldRenderer >> actualScreenSize [

	^ 240@120
]

{ #category : #'display box access' }
NullWorldRenderer >> checkForNewScreenSize [
]

{ #category : #utilities }
NullWorldRenderer >> convertWindowMouseEventPosition: aPosition [

	^ aPosition
]

{ #category : #private }
NullWorldRenderer >> deactivate [
]

{ #category : #private }
NullWorldRenderer >> doActivate [
]

{ #category : #operations }
NullWorldRenderer >> drawDuring: aBlock [

	| canvas |
	canvas := (Form extent: self actualScreenSize depth: 32) getCanvas.
	aBlock value: canvas
]

{ #category : #operations }
NullWorldRenderer >> forceDamageToScreen: allDamage [

	"Do Nothing"
]

{ #category : #accessing }
NullWorldRenderer >> icon: aForm [ 

	"Do nothing, a null world rendered has no window, so no icon either"
]

{ #category : #rendering }
NullWorldRenderer >> updateDamage: rectangles [

	"Do nothing"
]

{ #category : #events }
NullWorldRenderer >> updateToNewResolution [
]

{ #category : #accessing }
NullWorldRenderer >> windowTitle: aString [

	"I am a dummy implementation"
]
