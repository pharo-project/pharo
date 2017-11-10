"
I am activated when user clicks outside of a popup
"
Class {
	#name : #GLMPopupBrickOutisideClickPredicate,
	#superclass : #GLMPopupBrickPredicate,
	#category : #Glamour-Morphic-Brick-Widgets-UI-Popup
}

{ #category : #predicate }
GLMPopupBrickOutisideClickPredicate >> popup: aPopupBrick event: anEvent [
	"Return true when user clicks outside of a popup brick, false otherwise"
	<return: #Boolean>

	^ (anEvent type = #mouseDown) and: [
		(aPopupBrick isInsideOfPopupBrick: anEvent) not ]
]
