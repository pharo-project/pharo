"
I am a predicate that defines a boolean function used to decide whether an opener or closer should perform their actions
"
Class {
	#name : #GLMPopupBrickPredicate,
	#superclass : #Object,
	#category : #Glamour-Morphic-Brick-Widgets-UI-Popup
}

{ #category : #predicate }
GLMPopupBrickPredicate >> popup: aPopupBrick event: anEvent [
	"Return true if predicate is activated based on a given popup and event"
	<return: #Boolean>

	self subclassResponsibility
]
