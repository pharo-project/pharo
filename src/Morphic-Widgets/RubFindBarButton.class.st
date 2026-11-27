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
		'enabled'
	],
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

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
	c := self bounds center.
	lines := glyph caseOf: {
			         ([ #chevronRight ] -> [ { (-2 @ -4). (2 @ 0). (-2 @ 4) } ]).
			         ([ #chevronDown ] -> [ { (-4 @ -2). (0 @ 2). (4 @ -2) } ]).
			         ([ #up ] -> [ { (-4 @ 2). (0 @ -2). (4 @ 2) } ]).
			         ([ #down ] -> [ { (-4 @ -2). (0 @ 2). (4 @ -2) } ]).
			         ([ #close ] -> [ nil ]) }.
	lines ifNil: [
		aCanvas line: c + (-4 @ -4) to: c + (4 @ 4) width: 1 color: aColor.
		aCanvas line: c + (-4 @ 4) to: c + (4 @ -4) width: 1 color: aColor.
		^ self ].
	1 to: lines size - 1 do: [ :i | aCanvas line: c + (lines at: i) to: c + (lines at: i + 1) width: 1 color: aColor ]
]

{ #category : 'drawing' }
RubFindBarButton >> drawOn: aCanvas [

	| bounds fill textColor |
	bounds := self bounds.
	fill := selected
		        ifTrue: [ self theme selectionColor ]
		        ifFalse: [
			        (hovered and: [ enabled ])
				        ifTrue: [ self theme mouseOverColor ]
				        ifFalse: [ nil ] ].
	fill ifNotNil: [ aCanvas fillRectangle: bounds color: fill ].
	selected ifTrue: [ aCanvas frameRectangle: bounds width: 1 color: (self theme selectionColor muchDarker alpha: 0.6) ].
	textColor := enabled
		             ifTrue: [ self theme textColor ]
		             ifFalse: [ self theme ghostTextColor ].
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
		  ifNotNil: [ (self font widthOfString: label) + 12 max: self height ]
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
