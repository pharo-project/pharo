"
I am activated as soon as mouse is moved outside of a popup
"
Class {
	#name : #GLMPopupBrickUnhoverPredicate,
	#superclass : #GLMPopupBrickPredicate,
	#category : #'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #predicate }
GLMPopupBrickUnhoverPredicate >> popup: aPopupBrick event: anEvent [
	"Return true when mouse is moved outside of a popup brick, false otherwise"
	<return: #Boolean>
	
	^ (anEvent type = #mouseMove) and: [
		(aPopupBrick isInsideOfPopupBrick: anEvent) not ]
]
