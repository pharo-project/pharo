"
Similar to a ProportionalSplitterMorph but designed to attach to an edge of a single morph only.
"
Class {
	#name : #EdgeGripMorph,
	#superclass : #AbstractResizerMorph,
	#instVars : [
		'target',
		'edgeName',
		'fitTargetOwner'
	],
	#category : #'Morphic-Widgets-Windows-Resizing'
}

{ #category : #accessing }
EdgeGripMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	self fillStyle: self normalFillStyle
]

{ #category : #'accessing - layouts' }
EdgeGripMorph >> bottomLayoutFrame [
	"Answer the layout frame for a bottom edge."
	
	^ (0 @ 1 corner: 1 @ 1) asLayoutFrame 
		topLeftOffset: 22 @ SystemWindow borderWidth negated ;
		rightOffset: -22
]

{ #category : #defaults }
EdgeGripMorph >> defaultHeight [
	"Answer the default height for the receiver."
	
	^ProportionalSplitterMorph splitterWidth
]

{ #category : #defaults }
EdgeGripMorph >> defaultWidth [
	"Answer the default width for the receiver."
	
	^ProportionalSplitterMorph splitterWidth
]

{ #category : #accessing }
EdgeGripMorph >> edgeName [
	"Answer the value of edgeName"

	^ edgeName
]

{ #category : #accessing }
EdgeGripMorph >> edgeName: aSymbol [
	"Set the value of edgeName. This is the edge of the target
	that will be manipulated by the grip."

	edgeName := aSymbol.
	self
		setLayoutSizingFor: aSymbol;
		layoutFrame: self gripLayoutFrame;
		layoutChanged
]

{ #category : #geometry }
EdgeGripMorph >> extent: aPoint [
	"If our minor extent changes then adopt the pane colour to
	reflect any size based gradient in the theme.
	Assumes fillStyle will not change on the major extent for
	performance reasons."

	|ext|
	ext := self extent.
	super extent: aPoint.
	self isHorizontal
		ifTrue: [self extent y ~= ext y ifTrue: [
					self adoptPaneColor: self paneColor]]
		ifFalse: [self extent x ~= ext x ifTrue: [
					self adoptPaneColor: self paneColor]]
	
]

{ #category : #accessing }
EdgeGripMorph >> fitTargetBoundsInOwner: aRect [
	"Reset the target bounds if required to
	ensure that the owner's submorphs fit within the owner's
	exisiting bounds when layed out."

	|ownerMinExt targetOwner|
	targetOwner := self target owner ifNil: [^self].
	ownerMinExt :=  targetOwner minExtent.
	ownerMinExt x > self target owner width
		ifTrue: [self edgeName = #left
				ifTrue: [self target bounds: (aRect left + (ownerMinExt x - targetOwner width) @ aRect top extent: (aRect width - (ownerMinExt x - targetOwner width)) @ aRect height)]
				ifFalse: [self target bounds: (aRect origin extent: (aRect width - (ownerMinExt x - targetOwner width)) @ aRect height)]].
	ownerMinExt y > self target owner height
		ifTrue: [self edgeName = #top
				ifTrue: [self target bounds: (aRect left @ (aRect top + (ownerMinExt y - targetOwner height)) extent: aRect width @ (aRect height - (ownerMinExt y - targetOwner height)))]
				ifFalse: [self target bounds: (aRect origin extent: aRect width @ (aRect height - (ownerMinExt y - targetOwner height)))]]
		
	
]

{ #category : #accessing }
EdgeGripMorph >> fitTargetOwner [

	^ fitTargetOwner
]

{ #category : #accessing }
EdgeGripMorph >> fitTargetOwner: anObject [

	fitTargetOwner := anObject
]

{ #category : #'accessing - layouts' }
EdgeGripMorph >> gripLayoutFrame [
	"Answer the layout frame depending on our edge."
	
	self edgeName == #top ifTrue: [^self topLayoutFrame].
	self edgeName == #bottom ifTrue: [^self bottomLayoutFrame].
	self edgeName == #left ifTrue: [^self leftLayoutFrame].
	^self rightLayoutFrame
]

{ #category : #initialization }
EdgeGripMorph >> initialize [
	"Initialize the receiver."
	
	super initialize.
	self
		fitTargetOwner: false;
		edgeName: #right;
		extent: self defaultWidth @ self defaultHeight;
		hResizing: #spaceFill;
		vResizing: #spaceFill
]

{ #category : #testing }
EdgeGripMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout."
	
	^self edgeName == #top
		or: [self edgeName == #bottom]
]

{ #category : #'accessing - layouts' }
EdgeGripMorph >> leftLayoutFrame [
	"Answer the layout frame for a left edge."
	
	^ (0 @ 0 corner: 0 @ 1) asLayoutFrame 
		topOffset: -7; 
		bottomRightOffset: SystemWindow borderWidth @ (SystemWindow borderWidth - 26)
]

{ #category : #'event handling' }
EdgeGripMorph >> mouseDown: anEvent [
	"Remember the receiver and target offsets too."
	
	|cp|
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [self fillStyle: self pressedFillStyle].
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position. cp - self targetPoint}.
	self eventHandler
		ifNotNil: [self eventHandler mouseDown: anEvent fromMorph: self]

]

{ #category : #'event handling' }
EdgeGripMorph >> mouseMove: anEvent [
	"Track the mouse for resizing."
	
	target ifNil: [^self].
	self theme settings fastDragging
		ifTrue: [target doFastReframe: self edgeName] 
		ifFalse: [
			lastMouse at: 1 put: anEvent cursorPoint.
			self targetPoint: lastMouse first - lastMouse last.
			self positionPoint: (lastMouse first - lastMouse second)].
]

{ #category : #'event handling' }
EdgeGripMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [anEvent hand showTemporaryCursor: nil].
	self adoptPaneColor: self paneColor
]

{ #category : #accessing }
EdgeGripMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme splitterNormalFillStyleFor: self
]

{ #category : #accessing }
EdgeGripMorph >> positionPoint: aPoint [
	"Reposition based on ptName."
	
	(#(top bottom) includes: self edgeName)
		ifTrue: [^self position: self left @ aPoint y].
	(#(left right) includes: self edgeName)
		ifTrue: [^self position: aPoint x @ self top].
	^self position: aPoint
]

{ #category : #accessing }
EdgeGripMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^self theme splitterPressedFillStyleFor: self
]

{ #category : #actions }
EdgeGripMorph >> resizeCursor [

	^ Cursor resizeForEdge: self edgeName
]

{ #category : #'accessing - layouts' }
EdgeGripMorph >> rightLayoutFrame [
	"Answer the layout frame for a right edge."
	
	^ (1 @ 0 corner: 1 @ 1) asLayoutFrame 
		topLeftOffset: SystemWindow borderWidth negated @ -7 ;
		bottomOffset: SystemWindow borderWidth - 26
]

{ #category : #accessing }
EdgeGripMorph >> setLayoutSizingFor: aSymbol [
	"Adjust the sizing for use within table layouts."

	(aSymbol = #left or: [aSymbol = #right])
		ifTrue: [self hResizing: #rigid; vResizing: #spaceFill].
	(aSymbol = #top or: [aSymbol = #bottom])
		ifTrue: [self hResizing: #spaceFill; vResizing: #rigid]
]

{ #category : #accessing }
EdgeGripMorph >> setTargetBounds: aRect [
	"Set the target bounds, taking owner into account if required."

	self target bounds: aRect.
	self fitTargetOwner ifTrue: [
		self fitTargetBoundsInOwner: aRect]
]

{ #category : #testing }
EdgeGripMorph >> splitsTopAndBottom [
	"Answer true if the receiver has a horizontal layout."
	
	^self isHorizontal
]

{ #category : #accessing }
EdgeGripMorph >> target [
	"Answer the value of target"

	^ target
]

{ #category : #accessing }
EdgeGripMorph >> target: aMorph [
	"Set the value of target"

	target := aMorph
]

{ #category : #accessing }
EdgeGripMorph >> targetPoint [
	"Answer the reference point of the target."

	^self target bounds pointAtSideOrCorner: self edgeName
]

{ #category : #accessing }
EdgeGripMorph >> targetPoint: aPoint [
	"Set the reference point of the target."

	|minExt rect |
	rect := self target bounds withSideOrCorner: self edgeName setToPoint: aPoint.
	minExt := (self target layoutPolicy notNil and: [self target layoutPolicy isTableLayout])
		ifTrue: [self target layoutPolicy minExtentOf: self target in: self target layoutBounds]
		ifFalse: [self target minimumExtent].
	rect width <= minExt x ifTrue: [
		rect := self edgeName = #left
			ifTrue: [rect withSideOrCorner: #left setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect withSideOrCorner: #right setToPoint: self target bounds topLeft + minExt]].
	rect height <= minExt y ifTrue: [
		rect := self edgeName = #top
			ifTrue: [rect withSideOrCorner: #top setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect withSideOrCorner: #bottom setToPoint: self target bounds topLeft + minExt]].
	self setTargetBounds: rect
]

{ #category : #theme }
EdgeGripMorph >> themeChanged [
	"Update the fill style."

	self fillStyle: self normalFillStyle.
	super themeChanged
]

{ #category : #'accessing - layouts' }
EdgeGripMorph >> topLayoutFrame [
	"Answer the layout frame for a top edge."
	
	^ (0 @ 0 corner: 1 @ 0) asLayoutFrame 
		topLeftOffset: 22 @ -29 ;
		bottomRightOffset:  -22 @ (SystemWindow borderWidth - 29)
]
