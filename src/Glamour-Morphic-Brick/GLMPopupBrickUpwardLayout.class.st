"
I am a special upward popup layout.
I make sure that content is above the trigger brick
"
Class {
	#name : #GLMPopupBrickUpwardLayout,
	#superclass : #GLMPopupBrickLayout,
	#category : #Glamour-Morphic-Brick-Widgets-UI-Popup
}

{ #category : #building }
GLMPopupBrickUpwardLayout >> build: aPopup trigger: aTriggerBrick content: aContentBrick [

	aPopup removeAllBricks.
	aPopup contentBrick: aContentBrick.
	aPopup triggerBrick: aTriggerBrick
]

{ #category : #building }
GLMPopupBrickUpwardLayout >> open: aPopupBrick with: aRectangle [
	"In case of downward direction we want popup's content to be below trigger brick"

	^ aPopupBrick openAbove: aRectangle
]
