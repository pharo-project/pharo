"
I represents the windows frame for window dragging or resizing when fast dragging or fast resizing is wanted (when  UITheme currentSettings fastDragging is set to true).
For window resizing, I'm created by a corner or an edge grip when the mouse is clicked on it (see SystemWindow>>doFastWindowReframe:). For window dragging, I'm created when the top window bar is clicked (see SystemWindow>>doFastFrameDrag:).  I'm always created with the same bounds as the target window bounds. The mouse focus is given to me and my bounds are changed while the hand is moving. On mouse up, the window bounds is set to my own bounds and then I'm deleted.

Instance Variables
	location:		<Symbol>
	startGap:		<Point>
	target:		<SystemWindow>

location
	- The symbol representing the corner or the edge (#topLeft, #top, #topRight .... or #left). I'm set to nil for window dragging

startGap
	- For window dragging, keep track of the distance between the window top bar first click location and the window position

target
	- The window to be resized or dragged

"
Class {
	#name : #FastDraggingFrameMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'target',
		'startGap',
		'location'
	],
	#category : 'Morphic-Widgets-Windows-Widgets'
}

{ #category : #'instance creation' }
FastDraggingFrameMorph class >> forDragging: aWindow clickedAt: aPoint [
	| b |
	b := self new.
	b target: aWindow.
	b startGap: aWindow topLeft - aPoint.
	^ b
]

{ #category : #'instance creation' }
FastDraggingFrameMorph class >> forResizing: aWindow fromLocation: aSideOrCorner [
	| b  |
	b := self new.	
	b target: aWindow.
	b location: aSideOrCorner.
	^ b 
]

{ #category : #initialization }
FastDraggingFrameMorph >> defaultBorderColor [ 
	^ Color gray 
]

{ #category : #initialization }
FastDraggingFrameMorph >> defaultBorderWidth [
	^ 2.
	
]

{ #category : #initialization }
FastDraggingFrameMorph >> defaultColor [ 
	^ Color gray alpha: 0.15.
	
]

{ #category : #'event handling' }
FastDraggingFrameMorph >> draggedTo: aPoint [
	self position: aPoint + startGap

]

{ #category : #'event handling' }
FastDraggingFrameMorph >> handlesMouseDown: evt [
	^ true
]

{ #category : #testing }
FastDraggingFrameMorph >> isForDragging [
	^ location isNil
]

{ #category : #accessing }
FastDraggingFrameMorph >> location: aSymbol [
	"The symbol wich represents the corner or the edge grip location (#topLeft, #top ..., #bottomLeft or #left)"
	location := aSymbol
]

{ #category : #'event handling' }
FastDraggingFrameMorph >> mouseDown: evt [
	"Normally, should not be possible"
	self delete 
]

{ #category : #'event handling' }
FastDraggingFrameMorph >> mouseMove: evt [
	self isForDragging 
		ifTrue: [self draggedTo: evt position]
		ifFalse: [self reframedTo: evt position]

]

{ #category : #'event handling' }
FastDraggingFrameMorph >> mouseUp: evt [ 
	target
		ifNotNil: [Display
				deferUpdatesIn: Display boundingBox
				while: [target bounds: self bounds]].
	self delete
]

{ #category : #'event handling' }
FastDraggingFrameMorph >> reframedTo: aPoint [ 
	self
		bounds: (self bounds withSideOrCorner: location setToPoint: aPoint)
]

{ #category : #accessing }
FastDraggingFrameMorph >> startGap: aPoint [
	"APoint is the distance between the window position and the first click position" 
	startGap := aPoint
]

{ #category : #accessing }
FastDraggingFrameMorph >> target [
	^ target
]

{ #category : #accessing }
FastDraggingFrameMorph >> target: aSystemWindow [ 
	target := aSystemWindow.
	self color: (target paneColor alpha: 0.35).
	self bounds: aSystemWindow bounds.
	self currentHand newMouseFocus: self
]

{ #category : #accessing }
FastDraggingFrameMorph >> wantsToBeTopmost [
	"Answer if the receiver want to be one of the topmost objects in 
	its owner"
	^ true
]
