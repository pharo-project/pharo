"
I am a small flat button of the inline find and replace bar (see RubFindReplaceBar).

I show either a short text label (such as 'Aa') or a glyph that I draw myself with the theme text color (#chevronRight, #chevronDown, #up, #down, #close), so that I look right in both the light and the dark themes.

I can be a toggle (see #beToggle): a selected toggle is drawn with the theme selection color.
I only paint a background when I am hovered or selected, to keep the bar minimal.
"
Class {
	#name : 'RubFindBarButton',
	#superclass : 'Morph',
	#instVars : [
		'label',
		'glyph',
		'action',
		'isToggle',
		'selected',
		'hovered',
		'enabled',
		'isPill'
	],
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'drawing' }
RubFindBarButton class >> fillRounded: aRectangle radius: aNumber color: aColor on: aCanvas [
	"Fill aRectangle with rounded corners using only pixel aligned rectangles: crisp straight edges and symmetric corners, without the irregular edges of an aliased polygon"

	| rect spans cornerRows |
	rect := aRectangle rounded.
	spans := self rowSpansOf: rect radius: aNumber.
	cornerRows := (aNumber min: rect height // 2) rounded.
	"the rows of the corners one by one, the straight middle at once"
	1 to: spans size do: [ :i |
		(i <= cornerRows or: [ i > (spans size - cornerRows) ]) ifTrue: [
			aCanvas fillRectangle: ((spans at: i) first @ (rect top + i - 1) corner: (spans at: i) last + 1 @ (rect top + i)) color: aColor ] ].
	spans size > (cornerRows * 2) ifTrue: [
		aCanvas fillRectangle: (rect left @ (rect top + cornerRows) corner: rect right @ (rect bottom - cornerRows)) color: aColor ]
]

{ #category : 'drawing' }
RubFindBarButton class >> frameRounded: aRectangle radius: aNumber color: aColor on: aCanvas [
	"Draw a 1 pixel outline of aRectangle with rounded corners, pixel aligned: the outer shape minus the shape inset by one pixel, row by row"

	| rect outer inner |
	rect := aRectangle rounded.
	outer := self rowSpansOf: rect radius: aNumber.
	inner := self rowSpansOf: (rect insetBy: 1) radius: aNumber - 1.
	1 to: outer size do: [ :i |
		| span y |
		span := outer at: i.
		y := rect top + i - 1.
		(i = 1 or: [ i = outer size or: [ inner isEmpty ] ])
			ifTrue: [ aCanvas fillRectangle: (span first @ y corner: span last + 1 @ (y + 1)) color: aColor ]
			ifFalse: [
				| innerSpan |
				innerSpan := inner at: i - 1.
				aCanvas fillRectangle: (span first @ y corner: innerSpan first @ (y + 1)) color: aColor.
				aCanvas fillRectangle: (innerSpan last + 1 @ y corner: span last + 1 @ (y + 1)) color: aColor ] ]
]

{ #category : 'drawing' }
RubFindBarButton class >> rowSpansOf: aRectangle radius: aNumber [
	"Answer, for each pixel row of aRectangle, the interval of the x coordinates covered by aRectangle with corners of radius aNumber"

	| rect radius height |
	rect := aRectangle rounded.
	height := rect height.
	height < 1 ifTrue: [ ^ #(  ) ].
	radius := aNumber min: (rect width min: height) / 2.
	^ (0 to: height - 1) collect: [ :row |
		  | dy inset |
		  dy := row < radius
			        ifTrue: [ radius - row - 0.5 ]
			        ifFalse: [
				        row >= (height - radius)
					        ifTrue: [ row - (height - radius) + 0.5 ]
					        ifFalse: [ 0 ] ].
		  inset := dy > 0
			           ifTrue: [ (radius - (radius squared - dy squared max: 0) sqrt) rounded ]
			           ifFalse: [ 0 ].
		  rect left + inset to: rect right - 1 - inset ]
]

{ #category : 'instance creation' }
RubFindBarButton class >> glyph: aSymbol help: aString action: aBlock [

	^ self new
		  glyph: aSymbol;
		  setBalloonText: aString;
		  action: aBlock;
		  yourself
]

{ #category : 'instance creation' }
RubFindBarButton class >> label: aString help: aStringOrNil action: aBlock [

	^ self new
		  label: aString;
		  setBalloonText: aStringOrNil;
		  action: aBlock;
		  yourself
]

{ #category : 'accessing' }
RubFindBarButton >> action: aBlock [

	action := aBlock
]

{ #category : 'accessing' }
RubFindBarButton >> beToggle [

	isToggle := true
]

{ #category : 'drawing' }
RubFindBarButton >> drawGlyphOn: aCanvas color: aColor [

	| c lines |
	c := self bounds center rounded.
	lines := glyph caseOf: {
			         ([ #chevronRight ] -> [ { (-2 @ -5). (3 @ 0). (-2 @ 5) } ]).
			         ([ #chevronDown ] -> [ { (-5 @ -2). (0 @ 3). (5 @ -2) } ]).
			         ([ #up ] -> [ { (-5 @ 3). (0 @ -2). (5 @ 3) } ]).
			         ([ #down ] -> [ { (-5 @ -2). (0 @ 3). (5 @ -2) } ]).
			         ([ #close ] -> [ nil ]) }.
	lines ifNil: [
		aCanvas line: c + (-4 @ -4) to: c + (4 @ 4) width: 1 color: aColor.
		aCanvas line: c + (-4 @ 4) to: c + (4 @ -4) width: 1 color: aColor.
		^ self ].
	1 to: lines size - 1 do: [ :i | aCanvas line: c + (lines at: i) to: c + (lines at: i + 1) width: 1 color: aColor ]
]

{ #category : 'drawing' }
RubFindBarButton >> drawOn: aCanvas [
	| bounds fill border textColor accent |
	bounds := self bounds.
	accent := self theme selectionColor mixed: 0.6 with: self theme textColor.
	fill := nil.
	border := nil.
	(hovered and: [ enabled ]) ifTrue: [ fill := self theme textColor alpha: 0.12 ].
	selected ifTrue: [
		fill := accent alpha: 0.3.
		border := accent ].
	(isPill and: [ border isNil ]) ifTrue: [ border := self theme borderColor ].
	fill ifNotNil: [ self class fillRounded: bounds radius: 4 color: fill on: aCanvas ].
	border ifNotNil: [ self class frameRounded: bounds radius: 4 color: border on: aCanvas ].
	textColor := enabled
		             ifTrue: [ self theme textColor ]
		             ifFalse: [ self theme textColor alpha: 0.35 ].
	label ifNotNil: [
		| font width |
		font := self font.
		width := font widthOfString: label.
		aCanvas
			drawString: label
			at: (bounds center - ((width @ font height) / 2)) rounded
			font: font
			color: textColor ].
	glyph ifNotNil: [ self drawGlyphOn: aCanvas color: textColor ]
]

{ #category : 'accessing' }
RubFindBarButton >> enabled: aBoolean [

	enabled = aBoolean ifTrue: [ ^ self ].
	enabled := aBoolean.
	self changed
]

{ #category : 'accessing' }
RubFindBarButton >> font [

	^ self theme labelFont
]

{ #category : 'accessing' }
RubFindBarButton >> glyph: aSymbol [

	glyph := aSymbol.
	self changed
]

{ #category : 'event handling' }
RubFindBarButton >> handlesMouseDown: anEvent [

	^ true
]

{ #category : 'event handling' }
RubFindBarButton >> handlesMouseOver: anEvent [

	^ true
]

{ #category : 'initialization' }
RubFindBarButton >> initialize [

	super initialize.
	isToggle := false.
	selected := false.
	hovered := false.
	enabled := true.
	isPill := false.
	self color: Color transparent.
	self extent: 22 @ 22
]

{ #category : 'testing' }
RubFindBarButton >> isSelected [

	^ selected
]

{ #category : 'testing' }
RubFindBarButton >> isToggle [

	^ isToggle
]

{ #category : 'accessing' }
RubFindBarButton >> label [

	^ label
]

{ #category : 'accessing' }
RubFindBarButton >> label: aString [

	label := aString.
	self changed
]

{ #category : 'accessing' }
RubFindBarButton >> minimumWidth [
	"The width needed by my label, or my height for a glyph."

	^ label
		  ifNil: [ self height ]
		  ifNotNil: [ (self font widthOfString: label) + 16 max: self height ]
]

{ #category : 'event handling' }
RubFindBarButton >> mouseDown: anEvent [
	"Consume the click, so that it does not reach the text below"
]

{ #category : 'event handling' }
RubFindBarButton >> mouseEnter: anEvent [

	hovered := true.
	self changed
]

{ #category : 'event handling' }
RubFindBarButton >> mouseLeave: anEvent [

	hovered := false.
	self changed
]

{ #category : 'event handling' }
RubFindBarButton >> mouseUp: anEvent [

	(enabled and: [ self containsPoint: anEvent cursorPoint ]) ifTrue: [ self press ]
]

{ #category : 'actions' }
RubFindBarButton >> press [

	action ifNotNil: [ action value ]
]

{ #category : 'accessing' }
RubFindBarButton >> selected: aBoolean [

	selected = aBoolean ifTrue: [ ^ self ].
	selected := aBoolean.
	self changed
]

{ #category : 'accessing' }
RubFindBarButton >> bePill [
	"A text button with a visible outline, such as Replace"

	isPill := true
]
