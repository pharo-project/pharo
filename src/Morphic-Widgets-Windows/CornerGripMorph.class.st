"
I am the superclass of a hierarchy of morph specialized in allowing the user to resize windows.
"
Class {
	#name : #CornerGripMorph,
	#superclass : #AbstractResizerMorph,
	#instVars : [
		'target'
	],
	#category : #Morphic-Widgets-Windows-Resizing
}

{ #category : #accessing }
CornerGripMorph >> defaultHeight [
	^ 22
]

{ #category : #accessing }
CornerGripMorph >> defaultWidth [
	^ 22
]

{ #category : #initialization }
CornerGripMorph >> initialize [
	super initialize.
	self extent: self defaultWidth @ self defaultHeight.
	self layoutFrame: self gripLayoutFrame
]

{ #category : #event }
CornerGripMorph >> mouseDown: anEvent [
	"Remember the receiver and target offsets too."
	
	|cp|
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position. cp - self targetPoint}
]

{ #category : #event }
CornerGripMorph >> mouseMove: anEvent [
	"Track the mouse for resizing."
	
	target ifNil: [^ self].
	target fastFramingOn 
		ifTrue: [target doFastWindowReframe: self ptName] 
		ifFalse: [
			lastMouse at: 1 put: anEvent cursorPoint.
			self targetPoint: lastMouse first - lastMouse last.
			self position: (lastMouse first - lastMouse second)].
]

{ #category : #accessing }
CornerGripMorph >> target [
	"Answer the target."

	^target
]

{ #category : #accessing }
CornerGripMorph >> target: aMorph [

	target := aMorph.
	aMorph ifNotNil: [
		self fillStyle: (aMorph theme resizerGripNormalFillStyleFor: self)]
]

{ #category : #accessing }
CornerGripMorph >> targetPoint [
	"Answer the reference point of the target."

	^self target bounds pointAtSideOrCorner: self ptName
]

{ #category : #accessing }
CornerGripMorph >> targetPoint: aPoint [
	"Set the reference point of the target."

	|minExt rect|
	rect := self target bounds withSideOrCorner: self ptName setToPoint: aPoint.
	minExt := self target minimumExtent.
	rect width <= minExt x ifTrue: [
		(self ptName = #topLeft or: [self ptName = #bottomLeft])
			ifTrue: [rect := rect withSideOrCorner: #left setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect := rect withSideOrCorner: #right setToPoint: self target bounds topLeft + minExt]].
	rect height <= minExt y ifTrue: [
		(self ptName = #topLeft or: [self ptName = #topRight])
			ifTrue: [rect := rect withSideOrCorner: #top setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect := rect withSideOrCorner: #bottom setToPoint: self target bounds topLeft + minExt]].
	self target bounds: rect
]
