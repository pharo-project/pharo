"
Morph displaying opposing arrows.
"
Class {
	#name : #BracketMorph,
	#superclass : #Morph,
	#instVars : [
		'orientation'
	],
	#category : 'Morphic-Widgets-Scrolling'
}

{ #category : #drawing }
BracketMorph >> drawOn: aCanvas [
	"Draw triangles at the edges."
	
	|r|
	r := self horizontal
		ifTrue: [self bounds insetBy: (2@1 corner: 2@1)]
		ifFalse: [self bounds insetBy: (1@2 corner: 1@2)].
	aCanvas
		drawPolygon: (self leftOrTopVertices: self bounds)
		fillStyle: self borderColor;
		drawPolygon: (self leftOrTopVertices: r)
		fillStyle: self fillStyle;
		drawPolygon: (self rightOrBottomVertices: self bounds)
		fillStyle: self borderColor;
		drawPolygon: (self rightOrBottomVertices: r)
		fillStyle: self fillStyle
]

{ #category : #accessing }
BracketMorph >> horizontal [
	"Answer whether horizontal or vertical."
	
	^self orientation == #horizontal
]

{ #category : #accessing }
BracketMorph >> horizontal: aBoolean [
	"Set whether horizontal or vertical."
	
	^self orientation: (aBoolean ifTrue: [#horizontal] ifFalse: [#vertical])
]

{ #category : #initialization }
BracketMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		orientation: #horizontal
]

{ #category : #geometry }
BracketMorph >> leftOrTopVertices: r [
	"Answer the vertices for a left or top bracket in the given rectangle."
	
	^self orientation == #vertical
		ifTrue: [{r topLeft - (0@1). r left + (r height // 2 + (r height \\ 2))@(r center y - (r height + 1 \\ 2)).
				r left + (r height // 2 + (r height \\ 2))@(r center y). r bottomLeft}]
		ifFalse: [{r topLeft. (r center x - (r width + 1 \\ 2))@(r top + (r width // 2 + (r width \\ 2))).
				r center x@(r top + (r width // 2 + (r width \\ 2))). r topRight}]
]

{ #category : #accessing }
BracketMorph >> orientation [
	"Answer the value of orientation"

	^ orientation
]

{ #category : #accessing }
BracketMorph >> orientation: anObject [
	"Set the value of orientation"

	orientation := anObject.
	self changed
]

{ #category : #geometry }
BracketMorph >> rightOrBottomVertices: r [
	"Answer the vertices for a right or bottom bracket in the given rectangle."
	
	^self orientation == #vertical
		ifTrue: [{r topRight - (0@1). r right - (r height // 2 + (r height \\ 2))@(r center y - (r height + 1 \\ 2)).
				r right - (r height // 2 + (r height \\ 2))@(r center y). r bottomRight}]
		ifFalse: [{(r center x)@(r bottom - 1 - (r width // 2 + (r width \\ 2))).
				r center x @(r bottom - 1 - (r width // 2 + (r width \\ 2))). r bottomRight. r bottomLeft - (1@0)}]
]
