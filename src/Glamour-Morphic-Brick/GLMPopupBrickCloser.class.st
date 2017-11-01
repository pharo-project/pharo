"
I am a popup closer responsible for closing a popup if any of my predicates are activated as the result of an event
"
Class {
	#name : #GLMPopupBrickCloser,
	#superclass : #GLMPopupBrickListener,
	#category : 'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #action }
GLMPopupBrickCloser >> act [
	self popup close
]
