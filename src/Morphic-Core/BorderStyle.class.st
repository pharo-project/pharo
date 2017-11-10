"
See BorderedMorph

BorderedMorh new borderStyle: (BorderStyle inset width: 2); openInWorld.
"
Class {
	#name : #BorderStyle,
	#superclass : #Object,
	#classVars : [
		'Default'
	],
	#category : #Morphic-Core-Borders
}

{ #category : #'instance creation' }
BorderStyle class >> borderStyleChoices [
	"Answer the superset of all supported borderStyle symbols"

	^ #(simple inset raised complexAltFramed complexAltInset complexAltRaised complexFramed complexInset complexRaised)
]

{ #category : #'instance creation' }
BorderStyle class >> borderStyleForSymbol: sym [
	"Answer a border style corresponding to the given symbol"

	| aSymbol |
	aSymbol := sym == #none ifTrue: [#simple] ifFalse: [sym].
	^ self perform: aSymbol
]

{ #category : #'instance creation' }
BorderStyle class >> color: aColor width: aNumber [
	^self width: aNumber color: aColor
]

{ #category : #'instance creation' }
BorderStyle class >> default [
	^Default ifNil:[Default := self new]
]

{ #category : #'instance creation' }
BorderStyle class >> thinGray [
	^ self width: 1 color: Color gray
]

{ #category : #'instance creation' }
BorderStyle class >> width: aNumber [
	^self width: aNumber color: Smalltalk ui theme borderColor
]

{ #category : #comparing }
BorderStyle >> = aBorderStyle [
	^self species = aBorderStyle species
		and:[self style == aBorderStyle style
		and:[self width = aBorderStyle width
		and:[self color = aBorderStyle color]]].
]

{ #category : #accessing }
BorderStyle >> baseColor [
	^Color transparent
]

{ #category : #accessing }
BorderStyle >> baseColor: aColor [
	"Ignored"
]

{ #category : #accessing }
BorderStyle >> color [
	^Color transparent
]

{ #category : #accessing }
BorderStyle >> color: aColor [
	"Ignored"
]

{ #category : #accessing }
BorderStyle >> colorsAtCorners [
	^Array new: 4 withAll: self color
]

{ #category : #accessing }
BorderStyle >> dotOfSize: diameter forDirection: aDirection [
	| form |
	form := Form extent: diameter@diameter depth: Display depth.
	form getCanvas fillOval: form boundingBox color: self color.
	^form
]

{ #category : #drawing }
BorderStyle >> drawLineFrom: startPoint to: stopPoint on: aCanvas [
	^aCanvas line: startPoint to: stopPoint width: self width color: self color
]

{ #category : #drawing }
BorderStyle >> frameOval: aRectangle on: aCanvas [
	"Frame the given rectangle on aCanvas"
	aCanvas frameOval: aRectangle width: self width color: self color
]

{ #category : #drawing }
BorderStyle >> framePolygon: vertices on: aCanvas [
	"Frame the given rectangle on aCanvas"
	self framePolyline: vertices on: aCanvas.
	self drawLineFrom: vertices last to: vertices first on: aCanvas.
]

{ #category : #drawing }
BorderStyle >> framePolyline: vertices on: aCanvas [ 
	"Frame the given rectangle on aCanvas"

	| prev next |
	prev := vertices first.
	2 to: vertices size
		do: 
			[:i | 
			next := vertices at: i.
			self 
				drawLineFrom: prev
				to: next
				on: aCanvas.
			prev := next]
]

{ #category : #drawing }
BorderStyle >> frameRectangle: aRectangle on: aCanvas [
	"Frame the given rectangle on aCanvas"
	aCanvas frameRectangle: aRectangle width: self width color: self color
]

{ #category : #testing }
BorderStyle >> hasFillStyle [
	"Answer false."

	^false
]

{ #category : #comparing }
BorderStyle >> hash [
	"hash is implemented because #= is implemented"
	^self species hash bitXor: (self width hash bitXor: self color hash)
]

{ #category : #testing }
BorderStyle >> isBorderStyle [
	^true
]

{ #category : #testing }
BorderStyle >> isComplex [
	^false
]

{ #category : #testing }
BorderStyle >> isComposite [
	"Answer false."

	^false
]

{ #category : #printing }
BorderStyle >> printOn: aStream [
	"Print a description of the
	receiver on the given stream."

	self storeOn: aStream
]

{ #category : #initialize }
BorderStyle >> releaseCachedState [
	"Release any associated cached state"
]

{ #category : #printing }
BorderStyle >> storeOn: aStream [
	"Store a reconstructable representation of the
	receiver on the given stream."

	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' width: ';
		print: self width;
		nextPutAll: ' color: ';
		print: self color;
		nextPutAll: ')'
]

{ #category : #accessing }
BorderStyle >> style [
	^#none
]

{ #category : #'color tracking' }
BorderStyle >> trackColorFrom: aMorph [
	"If necessary, update our color to reflect a change in aMorphs color"
]

{ #category : #accessing }
BorderStyle >> width [
	^0
]

{ #category : #accessing }
BorderStyle >> width: aNumber [
	"Ignored"
]

{ #category : #accessing }
BorderStyle >> widthForRounding [
	^self width
]
