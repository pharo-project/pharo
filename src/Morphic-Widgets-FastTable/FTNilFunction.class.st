"
I am a Function that do nothing to use a Null Object Pattern.
	
Example (Should only be create by a FTTableMorph)
-------------------------------------------------

FTNilFunction table: (FTTableMorph with: (1 to: 200))

"
Class {
	#name : #FTNilFunction,
	#superclass : #FTFunction,
	#category : #'Morphic-Widgets-FastTable-Functions'
}

{ #category : #accessing }
FTNilFunction >> beExplicit [
	"Do nothing"
]

{ #category : #protocol }
FTNilFunction >> disable [
	"Do nothing"

	
]

{ #category : #testing }
FTNilFunction >> isExplicit [
	^ false
]

{ #category : #'event handling' }
FTNilFunction >> keyStroke: anEvent [
	"Do nothing"
	^ false
]

{ #category : #initialization }
FTNilFunction >> reset [
]

{ #category : #private }
FTNilFunction >> resizeContainerFrom: topLefPoint to: bottomRightPoint [
	"I do not change the size of the container."

	^ topLefPoint corner: bottomRightPoint
]

{ #category : #private }
FTNilFunction >> resizeWidget [
	"Do nothing"
]

{ #category : #accessing }
FTNilFunction >> showWidget [
	"Do nothing"
]
