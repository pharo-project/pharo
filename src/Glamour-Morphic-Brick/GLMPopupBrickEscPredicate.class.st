"
I am activated when user presses `Escape` key
"
Class {
	#name : #GLMPopupBrickEscPredicate,
	#superclass : #GLMPopupBrickPredicate,
	#category : #'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #predicate }
GLMPopupBrickEscPredicate >> popup: aPopupBrick event: anEvent [
	"Return true when user presses escape key, false otherwise"
	<return: #Boolean>

	^ (anEvent type == #keystroke) and: [ 
			anEvent keyCharacter = Character escape ]
]
