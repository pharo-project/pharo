"
A non user-moveable docking bar that uses theme-based fill styles.
"
Class {
	#name : #ToolDockingBarMorph,
	#superclass : #DockingBarMorph,
	#category : #'Polymorph-Widgets-Base'
}

{ #category : #accessing }
ToolDockingBarMorph >> gradientRamp [
	"If not set answer based on orininal color."
	
	^self normalFillStyle colorRamp
]

{ #category : #style }
ToolDockingBarMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme dockingBarNormalFillStyleFor: self
]

{ #category : #'private - layout' }
ToolDockingBarMorph >> updateColor [
	"private - update the receiver's color"
	
	self autoGradient
		ifFalse: [^ self].
	self fillStyle: self normalFillStyle
]

{ #category : #menu }
ToolDockingBarMorph >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu."
	
	^false
]
