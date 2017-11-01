"
I am a special downward popup layout.
I make sure that content is below the trigger brick
"
Class {
	#name : #GLMPopupBrickDownwardLayout,
	#superclass : #GLMPopupBrickLayout,
	#category : 'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #building }
GLMPopupBrickDownwardLayout >> build: aPopup trigger: aTriggerBrick content: aContentBrick [

	aPopup removeAllBricks.
	aPopup triggerBrick: aTriggerBrick.
	aPopup contentBrick: aContentBrick
]

{ #category : #opening }
GLMPopupBrickDownwardLayout >> open: aPopupBrick with: aRectangle [
	"In case of downward direction we want popup's content to be below trigger brick"

	^ aPopupBrick openBelow: aRectangle
]
