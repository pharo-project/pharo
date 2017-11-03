"
I am a popup layout.
My main responsibility is to position popup's elements and align them.

Popup layout can be:
- downward (trigger brick is on top following by content below, default)
- upward (trigger brick is beneath the content)

Additionally, popup can be aligned differently
- left-to-right (trigger and content are aligned to the right within bounding box, default)
- right-to-left (trigger and content are aligned to the left within bounding box)
"
Class {
	#name : #GLMPopupBrickLayout,
	#superclass : #Object,
	#category : #Glamour-Morphic-Brick-Widgets-UI-Popup
}

{ #category : #building }
GLMPopupBrickLayout >> build: aPopup trigger: aTriggerBrick content: aContentBrick [
	"Adds trigger and content bricks to a given popup based on my layout strategy"

	self subclassResponsibility
]

{ #category : #opening }
GLMPopupBrickLayout >> open: aPopupBrick with: aRectangle [
	"Open a given popup brick so that a trigger brick is located just above the provided
	rectangle in global coordinates. My subclasses should implement a concrete strategy based
	on layout direction (downward or upward).
	Return a morphic foundation brick used to wrap a popup"
	<return: #GLMFoundationBrick>

	^ self subclassResponsibility
]
