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
		'menuBlock',
		'branchButton'
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

{ #category : 'drawing' }
QuickProjectRowMorph class >> fit: aString font: aFont width: anInteger [
	"Cut a text that does not fit at the end, with an ellipsis, rather than in the middle of a letter."

	| text |
	(aFont widthOfString: aString) <= anInteger ifTrue: [ ^ aString ].
	text := aString.
	[ text notEmpty and: [ (aFont widthOfString: text , '...') > anInteger ] ]
		whileTrue: [ text := text allButLast ].
	^ text , '...'
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

{ #category : 'geometry' }
QuickProjectRowMorph >> actionColumnWidth [
	"Wide enough for every action, so the branch icons line up from row to row."

	^ 76
]

{ #category : 'drawing' }
QuickProjectRowMorph >> actionFont [

	^ StandardFonts defaultFont emphasized: TextEmphasis bold emphasisCode
]

{ #category : 'accessing' }
QuickProjectRowMorph >> branchBlock: aBlock [

	"Always shown: an inactive window gets no hover, and this must work next to the window being used."

	self isBusy ifTrue: [ ^ self ].
	branchButton := QuickProjectBranchChipMorph
		label: entry displayBranch
		action: aBlock.
	self addMorphBack: branchButton.
	self placeBranchButton
]

{ #category : 'accessing' }
QuickProjectRowMorph >> branchButton [

	^ branchButton
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
	textRight := self left + 50 + self textWidth.
	font := StandardFonts defaultFont.
	smallFont := QuickProjectBranchChipMorph font.
	aCanvas
		drawString: (self fit: entry name font: font width: self textWidth)
		in: (textLeft @ (self top + 7) corner: textRight @ self center y + 2)
		font: (font emphasized: TextEmphasis bold emphasisCode)
		color: self nameColor.
	aCanvas
		drawString: (self fit: entry displayRepository font: smallFont width: self textWidth)
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

{ #category : 'geometry' }
QuickProjectRowMorph >> extent: aPoint [

	super extent: aPoint.
	self placeBranchButton
]

{ #category : 'drawing' }
QuickProjectRowMorph >> fit: aString font: aFont width: anInteger [

	^ self class fit: aString font: aFont width: anInteger
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

	"Going onto the branch button also counts as leaving the row."
	(anEvent notNil and: [ self containsPoint: (self globalPointToLocal: anEvent hand position) ])
		ifTrue: [ ^ self ].
	hovering := false.
	self changed
]

{ #category : 'geometry' }
QuickProjectRowMorph >> placeBranchButton [

	branchButton ifNil: [ ^ self ].
	branchButton position: (self right - self actionColumnWidth - QuickProjectBranchChipMorph chipWidth - 2) @ (self center y - (branchButton height // 2))
]

{ #category : 'geometry' }
QuickProjectRowMorph >> textWidth [

	^ self width - 50 - self actionColumnWidth - QuickProjectBranchChipMorph chipWidth - 8
]

{ #category : 'drawing' }
QuickProjectRowMorph >> nameColor [
	"What is missing from this image is what the panel is for: projects already here step back."

	^ self isPrimaryAction
		ifTrue: [ self theme textColor ]
		ifFalse: [ self theme disabledTextColor ]
]
