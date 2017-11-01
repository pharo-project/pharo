"
Window edge gripper allowing resizing of a window by a particular side.
"
Class {
	#name : #WindowEdgeGripMorph,
	#superclass : #EdgeGripMorph,
	#category : 'Morphic-Widgets-Windows-Resizing'
}

{ #category : #'event handling' }
WindowEdgeGripMorph >> mouseDown: anEvent [
	"Activate the window if not currently so."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [self window ifNotNil: [:w | w activate]].
	^super mouseDown: anEvent
]

{ #category : #'event handling' }
WindowEdgeGripMorph >> mouseMove: anEvent [
	"Track the mouse for resizing."
	
	target ifNil: [^self].
	self theme settings fastDragging
		ifTrue: [target doFastWindowReframe: self edgeName] 
		ifFalse: [super mouseMove: anEvent]
]

{ #category : #style }
WindowEdgeGripMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme windowEdgeNormalFillStyleFor: self
]

{ #category : #style }
WindowEdgeGripMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^self theme windowEdgePressedFillStyleFor: self
]
