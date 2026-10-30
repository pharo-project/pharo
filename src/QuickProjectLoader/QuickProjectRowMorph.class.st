"
I am one row of the Quick Project Loader: a project name above its 'owner/repo', with its load action on the right.

I am drawn in one go with flat theme colors, so a row costs a single morph and no layout.
"
Class {
	#name : 'QuickProjectRowMorph',
	#superclass : 'Morph',
	#instVars : [
		'entry',
		'actionLabel',
		'hovering',
		'loadBlock',
		'menuBlock'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'instance creation' }
QuickProjectRowMorph class >> entry: anEntry action: aString onLoad: aBlock onMenu: anotherBlock [

	^ self new
		entry: anEntry;
		actionLabel: aString;
		loadBlock: aBlock;
		menuBlock: anotherBlock;
		yourself
]

{ #category : 'accessing' }
QuickProjectRowMorph class >> rowHeight [

	^ 46
]

{ #category : 'accessing' }
QuickProjectRowMorph >> actionLabel [

	^ actionLabel
]

{ #category : 'accessing' }
QuickProjectRowMorph >> actionLabel: aString [

	actionLabel := aString
]

{ #category : 'geometry' }
QuickProjectRowMorph >> actionBounds [
	"The area of the action, right-aligned and vertically centered."

	| width |
	width := (self actionFont widthOfString: actionLabel) + 22.
	^ (self right - width - 10) @ (self center y - 12) extent: width @ 24
]

{ #category : 'drawing' }
QuickProjectRowMorph >> actionFont [

	^ StandardFonts defaultFont emphasized: TextEmphasis bold emphasisCode
]

{ #category : 'drawing' }
QuickProjectRowMorph >> drawActionOn: aCanvas [

	| area theme labelOrigin |
	area := self actionBounds.
	theme := self theme.
	labelOrigin := area center - ((self actionFont widthOfString: actionLabel) @ self actionFont height // 2).
	self isPrimaryAction ifTrue: [
		aCanvas
			roundCornersOf: self
			in: area
			during: [ aCanvas fillRectangle: area color: theme selectionColor ].
		^ aCanvas
			drawString: actionLabel
			at: labelOrigin
			font: self actionFont
			color: theme selectionTextColor ].
	(hovering or: [ self isBusy ]) ifTrue: [
		aCanvas
			drawString: actionLabel
			at: labelOrigin
			font: self actionFont
			color: theme ghostTextColor ]
]

{ #category : 'drawing' }
QuickProjectRowMorph >> drawInitialOn: aCanvas [

	| area font initial theme |
	theme := self theme.
	font := self actionFont.
	area := (self left + 10) @ (self center y - 15) extent: 30 @ 30.
	initial := entry name ifEmpty: [ '?' ] ifNotEmpty: [ :name | name first asUppercase asString ].
	aCanvas
		roundCornersOf: self
		in: area
		during: [ aCanvas fillRectangle: area color: theme lightBackgroundColor ].
	aCanvas
		drawString: initial
		at: area center - ((font widthOfString: initial) @ font height // 2)
		font: font
		color: self nameColor
]

{ #category : 'drawing' }
QuickProjectRowMorph >> drawOn: aCanvas [

	| textLeft textRight font smallFont |
	hovering ifTrue: [ aCanvas fillRectangle: self bounds color: self theme lightBackgroundColor ].
	self drawInitialOn: aCanvas.

	textLeft := self left + 50.
	textRight := self actionBounds left - 8.
	font := StandardFonts defaultFont.
	smallFont := font copy pointSize: (font pointSize - 1 max: 8).
	aCanvas
		drawString: entry name
		in: (textLeft @ (self top + 7) corner: textRight @ self center y + 2)
		font: (font emphasized: TextEmphasis bold emphasisCode)
		color: self nameColor.
	aCanvas
		drawString: entry displaySource
		in: (textLeft @ (self center y + 2) corner: textRight @ self bottom)
		font: smallFont
		color: self theme ghostTextColor.

	self drawActionOn: aCanvas
]

{ #category : 'accessing' }
QuickProjectRowMorph >> entry [

	^ entry
]

{ #category : 'accessing' }
QuickProjectRowMorph >> entry: anEntry [

	entry := anEntry
]

{ #category : 'event handling' }
QuickProjectRowMorph >> handlesMouseDown: anEvent [

	^ true
]

{ #category : 'event handling' }
QuickProjectRowMorph >> handlesMouseOver: anEvent [

	^ true
]

{ #category : 'initialization' }
QuickProjectRowMorph >> initialize [

	super initialize.
	hovering := false.
	actionLabel := ''.
	self
		color: Color transparent;
		borderWidth: 0;
		useRoundedCorners;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: self class rowHeight
]

{ #category : 'testing' }
QuickProjectRowMorph >> isBusy [

	^ actionLabel = 'Loading...'
]

{ #category : 'testing' }
QuickProjectRowMorph >> isPrimaryAction [

	^ actionLabel = 'Load'
]

{ #category : 'accessing' }
QuickProjectRowMorph >> loadBlock: aBlock [

	loadBlock := aBlock
]

{ #category : 'accessing' }
QuickProjectRowMorph >> menuBlock: aBlock [

	menuBlock := aBlock
]

{ #category : 'event handling' }
QuickProjectRowMorph >> mouseDown: anEvent [

	anEvent yellowButtonPressed ifTrue: [ ^ menuBlock cull: anEvent ].
	(self isBusy not and: [ self actionBounds containsPoint: anEvent position ])
		ifTrue: [ loadBlock value ]
]

{ #category : 'event handling' }
QuickProjectRowMorph >> mouseEnter: anEvent [

	hovering := true.
	self changed
]

{ #category : 'event handling' }
QuickProjectRowMorph >> mouseLeave: anEvent [

	hovering := false.
	self changed
]

{ #category : 'drawing' }
QuickProjectRowMorph >> nameColor [
	"What is missing from this image is what the panel is for: projects already here step back."

	^ self isPrimaryAction
		ifTrue: [ self theme textColor ]
		ifFalse: [ self theme disabledTextColor ]
]
