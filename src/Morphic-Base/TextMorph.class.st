"
TextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text.

Late in life, TextMorph was made a subclass of BorderedMorph to provide border and background color if desired.  In order to keep things compatible, protocols have been redirected so that color (preferably textColor) relates to the text, and backgroundColor relates to the inner fill color.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.

If text has been embedded in another object, one can elect to fill the owner's shape, in which case the text will be laid out in the shape of the owner's shadow image (including any submorphs other than the text).  One can also elect to have the text avoid occlusions, in which case it will avoid the bounds of any sibling morphs that appear in front of it.  It may be necessary to update bounds in order for the text runaround to notice the presence of a new occluding shape.

The optional autoFitContents property enables the following feature:  if the text contents changes, then the bounds of the morph will be adjusted to fit the minimum rectangle that encloses the text (plus any margins specified).  Similarly, any attempt to change the size of the morph will be resisted if this parameter is set.  Except...

If the wrapFlag parameter is true, then text will be wrapped at word boundaries based on the composition width (innerBounds insetBy: margins) width.  Thus an attempt to resize the morph in autofit mode, if it changes the width, will cause the text to be recomposed with the new width, and then the bounds will be reset to the minimum enclosing rectangle.  Similarly, if the text contents are changed with the wrapFlag set to true, word wrap will be performed based on the current compostion width, after which the bounds will be set (or not), based on the autoFitcontents property.

Note that fonts can only be applied to the TextMorph as a whole.  While you can change the size, color, and emphasis of a subsection of the text and have it apply to only that subsection, changing the font changes the font for the entire contents of the TextMorph. 

Still a TextMorph can be composed of several texts of different fonts
| font1 font2 t1 t2 tMorph|
tMorph := TextMorph new.
font1 := (TextFontReference toFont: (StrikeFont familyName: 'Atlanta' size: 22)).
font2 := (TextFontReference toFont: (StrikeFont familyName: 'Atlanta' size: 11)).
t1 := 'this is font1' asText addAttribute: font1.
t2 := ' and this is font2' asText addAttribute: font2.
tMorph contents: (t1,t2).
tMorph openInHand.


Yet to do:
Make a comprehensive control for the eyedropper, with border width and color, inner color and text color, and margin widths.
"
Class {
	#name : #TextMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'textStyle',
		'text',
		'wrapFlag',
		'paragraph',
		'editor',
		'container',
		'predecessor',
		'successor',
		'backgroundColor',
		'margins',
		'defaultColor'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #shortcuts }
TextMorph class >> buildTextEditorKeymapsOn: aBuilder [
	<keymap>
	
	(aBuilder shortcut: #accept)
		category: #TextMorph
		default: PharoShortcuts current acceptShortcut
		do: [ :morph | morph acceptContents ].
]

{ #category : #editing }
TextMorph >> acceptContents [
	"The message is sent when the user hits enter or Cmd-S.
	Accept the current contents and end editing.
	This default implementation does nothing."
	self updateFromParagraph
]

{ #category : #layout }
TextMorph >> acceptDroppingMorph: aMorph event: evt [
	"This message is sent when a morph is dropped onto me."

	self addMorphFront: aMorph fromWorldPosition: aMorph position.
		"Make a TextAnchor and install it in a run."
]

{ #category : #editing }
TextMorph >> acceptOnCR [
	"Answer whether the receiver wants to accept when the Return key is hit.  Generic TextMorph has no such feature, but subclasses may."

	^ false
]

{ #category : #menu }
TextMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	| outer |
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addUpdating: #autoFitString target: self selector: #autoFitOnOff.
	aCustomMenu addUpdating: #wrapString target: self selector: #wrapOnOff.
	aCustomMenu add: 'text margins...' selector: #changeMargins:.
	aCustomMenu add: 'add predecessor' selector: #addPredecessor:.
	aCustomMenu add: 'add successor' selector: #addSuccessor:.
	aCustomMenu add: 'code pane menu...' selector: #yellowButtonActivity.
	aCustomMenu add: 'code pane shift menu...' selector: #shiftedYellowButtonActivity.

	outer := self owner.
	outer ifNotNil: [
	outer isLineMorph ifTrue:
		[container isNil
			ifFalse: [aCustomMenu add: 'reverse direction' selector: #reverseCurveDirection.
					aCustomMenu add: 'set baseline' selector: #setCurveBaseline:]]
		ifFalse:
		[self fillsOwner
			ifFalse: [aCustomMenu add: 'fill owner''s shape' selector: #fillingOnOff]
			ifTrue: [aCustomMenu add: 'rectangular bounds' selector: #fillingOnOff].
		self avoidsOcclusions
			ifFalse: [aCustomMenu add: 'avoid occlusions' selector: #occlusionsOnOff]
			ifTrue: [aCustomMenu add: 'ignore occlusions' selector: #occlusionsOnOff]]].
	
]

{ #category : #'submorphs-add/remove' }
TextMorph >> addMorphFront: aMorph fromWorldPosition: wp [ 
	"Overridden for more specific re-layout and positioning"
	aMorph textAnchorType == #document 
		ifFalse:[^self anchorMorph: aMorph at: wp type: aMorph textAnchorType].
	self addMorphFront: aMorph.

]

{ #category : #'linked frames' }
TextMorph >> addPredecessor: evt [
	| newMorph |
	newMorph := self copy predecessor: predecessor successor: self.
	newMorph extent: self width @ 100.
	predecessor ifNotNil: [predecessor setSuccessor: newMorph].
	self setPredecessor: newMorph.
	predecessor recomposeChain.
	evt hand attachMorph: newMorph
]

{ #category : #'linked frames' }
TextMorph >> addSuccessor: evt [
	| newMorph |
	newMorph := self copy predecessor: self successor: successor.
	newMorph extent: self width @ 100.
	successor ifNotNil: [successor setPredecessor: newMorph].
	self setSuccessor: newMorph.
	successor recomposeChain.
	evt hand attachMorph: newMorph
]

{ #category : #private }
TextMorph >> adjustLineIndicesBy: delta [
	paragraph ifNotNil: [paragraph adjustLineIndicesBy: delta]
]

{ #category : #anchors }
TextMorph >> anchorMorph: aMorph at: aPoint type: anchorType [
	| relPt index newText block |
	aMorph owner == self ifTrue:[self removeMorph: aMorph].
	aMorph textAnchorType: nil.
	aMorph relativeTextAnchorPosition: nil.
	self addMorphFront: aMorph.
	aMorph textAnchorType: anchorType.
	aMorph relativeTextAnchorPosition: nil.
	anchorType == #document ifTrue:[^self].
	relPt := self transformFromWorld globalPointToLocal: aPoint.
	index := (self paragraph characterBlockAtPoint: relPt) stringIndex.
	newText := Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: aMorph).
	anchorType == #inline ifTrue:[
		self paragraph replaceFrom: index to: index-1 with: newText displaying: false.
	] ifFalse:[
		index := index min: paragraph text size.
		index := paragraph text string lastIndexOf: Character cr startingAt: index ifAbsent:[0].
		block := paragraph characterBlockForIndex: index+1.
		aMorph relativeTextAnchorPosition: (relPt x - bounds left) @ (relPt y - block top ).
		self paragraph replaceFrom: index+1 to: index with: newText displaying: false.
	].
	self fit.
]

{ #category : #drawing }
TextMorph >> areasRemainingToFill: aRectangle [
	"Overridden from BorderedMorph to test backgroundColor instead of (text) color."
	(backgroundColor isNil or: [backgroundColor isTranslucent])
		ifTrue: [^ Array with: aRectangle].
	self wantsRoundedCorners
	ifTrue: [(borderWidth > 0 and: [borderColor isColor and: [borderColor isTranslucent]])
				ifTrue: [^ aRectangle areasOutside: (self innerBounds intersect: self boundsWithinCorners ifNone: [self error: 'cannot happen' ])]
				ifFalse: [^ aRectangle areasOutside: self boundsWithinCorners]]
	ifFalse: [(borderWidth > 0 and: [borderColor isColor and: [borderColor isTranslucent]])
				ifTrue: [^ aRectangle areasOutside: self innerBounds]
				ifFalse: [^ aRectangle areasOutside: self bounds]]
]

{ #category : #accessing }
TextMorph >> asText [
	^ text
]

{ #category : #accessing }
TextMorph >> autoFit: trueOrFalse [
	"Should I automatically adjust my size to fit text as it changes?"

	self isAutoFit = trueOrFalse ifTrue: [^ self].
	self autoFitOnOff
]

{ #category : #menu }
TextMorph >> autoFitOnOff [
	self setProperty: #autoFitContents toValue: self isAutoFit not.
	self isAutoFit ifTrue: [self fit]
]

{ #category : #menu }
TextMorph >> autoFitString [
	"Answer the string to put in a menu that will invite the user to 
	switch autoFit mode"
	^ (self isAutoFit
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text auto fit' translated
]

{ #category : #containment }
TextMorph >> avoidsOcclusions [
	^container notNil and: [ container avoidsOcclusions ]

]

{ #category : #accessing }
TextMorph >> backgroundColor [
	^ backgroundColor
]

{ #category : #accessing }
TextMorph >> backgroundColor: newColor [
	backgroundColor := newColor.
	self changed
]

{ #category : #'event handling' }
TextMorph >> basicKeyStroke: evt [ 
	"Handle a keystroke event."
	| action |

	evt keyValue = 13
		ifTrue: [
			action := self crAction.
			action
				ifNotNil: ["Note: Code below assumes that this was some 
					input field reacting on CR. Break the keyboard 
					focus so that the receiver can be safely deleted."
					evt hand newKeyboardFocus: nil.
					^ action value]].
	self handleInteraction: [ editor keystroke: evt ].
	self updateFromParagraph.
	super keyStroke: evt.
]

{ #category : #initialization }
TextMorph >> beAllFont: aFont [

	textStyle := TextStyle fontArray: (Array with: aFont).
	self releaseCachedState; changed
]

{ #category : #blinking }
TextMorph >> blinkStart [
	"Reset time for blink cursor after which blinking should actually start"
	^self valueOfProperty: #blinkStart ifAbsent:[Time millisecondClockValue]

]

{ #category : #blinking }
TextMorph >> blinkStart: msecs [
	"Reset time for blink cursor after which blinking should actually start"
	^self setProperty: #blinkStart toValue: msecs

]

{ #category : #'editing helpers' }
TextMorph >> bold [
	
	self changeEmphasis: #bold
]

{ #category : #accessing }
TextMorph >> borderWidth: newWidth [
	super borderWidth: newWidth.
	paragraph ifNotNil: [self composeToBounds].
]

{ #category : #geometry }
TextMorph >> bounds [
	container ifNil: [^ bounds].
	^ container bounds ifNil: [bounds]
]

{ #category : #testing }
TextMorph >> canChangeText [
	^ self enabled 
]

{ #category : #editing }
TextMorph >> cancelEdits [
	"The message is sent when the user hits enter or Cmd-L.
	Cancel the current contents and end editing.
	This default implementation does nothing."
	self releaseParagraph
]

{ #category : #alignment }
TextMorph >> centered [ 
	self changeAlignment: #centered
]

{ #category : #'editing helpers' }
TextMorph >> changeAlignment: aSymbol [
	"Change the alignment of the receiver. Alignment can be #leftFlush #centered #rightFlush #justified "
	
	"| t |
t := 'khkjhjkhjkhjkh
kjhj
kjhkjhkjh
kjh
kjhkjh kjh jh jh jh jh kjh kjh ' asTextMorph.
t openInWorld.
t selectAll; changeFormatting: #rightFlush.
t inspect"
	
	self editor applyAttribute: (TextAlignment perform: aSymbol).
	self updateFromParagraph
]

{ #category : #'editing helpers' }
TextMorph >> changeEmphasis: aSymbol [
	"Change the alignment of the receiver. Alignment can be #normal, #bold,  #italic, #narrow, #underlined, or #struckOut"
	
	"| t |
t := 'khkjhjkhjkhjkh
kjhj
kjhkjhkjh
kjh
kjhkjh kjh jh jh jh jh kjh kjh ' asTextMorph.
t openInWorld.
t selectAll; changeEmphasis: #bold.
t inspect"
	
	self editor applyAttribute: (TextEmphasis perform: aSymbol).
	"this code should be moved to TextEditor but changeEmphasis: is defined there for something else
	so it should be renamed first there."
	self updateFromParagraph
]

{ #category : #menu }
TextMorph >> changeMargins: evt [
	| handle origin aHand newMargin |
	aHand := evt ifNil: [self primaryHand] ifNotNil: [evt hand].
	origin := aHand position.
	handle := HandleMorph new
		forEachPointDo:
			[:newPoint | handle removeAllMorphs.
			handle addMorph:
				(LineMorph from: origin to: newPoint color: Color black width: 1).
			newMargin := (newPoint - origin max: 0@0) // 5.
			self margins: newMargin asMargin]
		lastPointDo:
			[:newPoint | handle deleteBalloon.
			self halo ifNotNil: [:halo | halo addHandles]].
	aHand attachMorph: handle.
	handle showBalloon:
'Move cursor down and to the right
to increase margin inset.
Click when done.' hand: evt hand.
	handle startStepping
]

{ #category : #editing }
TextMorph >> chooseAlignment [
	"Interactively change alignment."
	
	self editor changeAlignment.
	self updateFromParagraph
]

{ #category : #editing }
TextMorph >> chooseEmphasis [
	"Interactively change emphasis."
	
	self editor changeEmphasis.
	self updateFromParagraph
]

{ #category : #editing }
TextMorph >> chooseEmphasisOrAlignment [
	self editor changeEmphasisOrAlignment.
	self updateFromParagraph
]

{ #category : #editing }
TextMorph >> chooseFont [
	self editor changeTextFont.
	self updateFromParagraph.
]

{ #category : #editing }
TextMorph >> chooseStyle [
	self editor changeStyle.
	self updateFromParagraph.
]

{ #category : #private }
TextMorph >> clippingRectangle [
	^ self innerBounds
]

{ #category : #private }
TextMorph >> composeToBounds [
	"Compose my text to fit my bounds.
	If any text lies outside my bounds, it will be clipped, or
	if I have successors, it will be shown in the successors."
	| |
	self releaseParagraph; paragraph.
	container ifNotNil:
		[self privateBounds: container bounds truncated].
	self paragraph positionWhenComposed: self position.
	successor ifNotNil:
		[successor predecessorChanged].


]

{ #category : #private }
TextMorph >> compositionRectangle [
	| compRect |
	compRect := self innerBounds.
	compRect := compRect insetBy: margins.
	compRect width < 9 ifTrue: [compRect := compRect withWidth: 9].
	compRect height < 16 ifTrue: [compRect := compRect withHeight: 16].
	^ compRect
]

{ #category : #geometry }
TextMorph >> container [
	"Return the container for composing this text.  There are four cases:
	1.  container is specified as, eg, an arbitrary shape,
	2.  container is specified as the bound rectangle, because
		this morph is linked to others,
	3.  container is nil, and wrap is true -- grow downward as necessary,
	4.  container is nil, and wrap is false -- grow in 2D as nexessary."

	container ifNil:
		[successor ifNotNil: [^ self compositionRectangle].
		wrapFlag ifTrue: [^ self compositionRectangle withHeight: 9999999].
		^ self compositionRectangle topLeft extent: 9999999@9999999].
	^ container
]

{ #category : #'geometry testing' }
TextMorph >> containsPoint: aPoint [
	(super containsPoint: aPoint) ifFalse: [^ false].  "Not in my bounds"
	container ifNil: [^ true].  "In bounds of simple text"
	self startingIndex > text size ifTrue:
		["make null text frame visible"
		^ super containsPoint: aPoint].
	"In complex text (non-rect container), test by line bounds"
	^ self paragraph containsPoint: aPoint

]

{ #category : #accessing }
TextMorph >> contents [

	^ text
]

{ #category : #accessing }
TextMorph >> contents: stringOrText [
	^ self contentsAsIs: stringOrText
]

{ #category : #accessing }
TextMorph >> contents: stringOrText wrappedTo: width [
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	self newContents: ''.
	wrapFlag := true.
	super extent: width truncated@self height.
	self newContents: stringOrText
]

{ #category : #accessing }
TextMorph >> contentsAsIs: stringOrText [
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	wrapFlag := false.
	container ifNotNil: [container fillsOwner ifTrue: [wrapFlag := true]].
	self newContents: stringOrText
]

{ #category : #accessing }
TextMorph >> contentsWrapped: stringOrText [
	"Accept new text contents.  Lay it out, wrapping within my current width.
	Then fit my height to the result."
	wrapFlag := true.
	self newContents: stringOrText
]

{ #category : #copying }
TextMorph >> copy [
	^ super copy text: text copy textStyle: textStyle copy 
		wrap: wrapFlag color: color
		predecessor: nil successor: nil
]

{ #category : #'interactive error protocol' }
TextMorph >> correctFrom: start to: stop with: aString [
	editor ifNotNil: [ editor correctFrom: start to: stop with: aString ]
]

{ #category : #accessing }
TextMorph >> crAction [
	"Return the action to perform when encountering a CR in the input"
	^self valueOfProperty: #crAction
]

{ #category : #accessing }
TextMorph >> crAction: aMessageSend [
	"Return the action to perform when encountering a CR in the input"
	^self setProperty: #crAction toValue: aMessageSend
]

{ #category : #accessing }
TextMorph >> cursor [
	"Answer the receiver's logical cursor position"

	| loc |
	loc := self valueOfProperty: #textCursorLocation  ifAbsentPut: [1].
	loc := loc min: text string size.
	^ loc rounded
	
]

{ #category : #accessing }
TextMorph >> cursorWrapped: aNumber [
	"Set the cursor as indicated"

	self setProperty: #textCursorLocation toValue: (((aNumber rounded - 1) \\  text string size) + 1)

	
]

{ #category : #drawing }
TextMorph >> debugDrawLineRectsOn: aCanvas [
	"Shows where text line rectangles are"
	self paragraph lines do:
		[:line | aCanvas frameRectangle: line rectangle color: Color brown]

]

{ #category : #initialization }
TextMorph >> defaultColor [

	^ defaultColor ifNil: [ self theme textColor ]
]

{ #category : #geometry }
TextMorph >> defaultLineHeight [
	^ textStyle lineGrid
]

{ #category : #'submorphs-add/remove' }
TextMorph >> delete [
	predecessor ifNotNil: [predecessor setSuccessor: successor].
	successor ifNotNil: [successor setPredecessor: predecessor.
						successor recomposeChain].
	super delete
]

{ #category : #'interactive error protocol' }
TextMorph >> deselect [
	editor ifNotNil: [ editor deselect ]
]

{ #category : #drawing }
TextMorph >> drawNullTextOn: aCanvas [
	"Make null text frame visible.
	Nicer if not shaded!"

	aCanvas fillRectangle: bounds color: (self backgroundColor ifNil: [Color transparent])
]

{ #category : #drawing }
TextMorph >> drawOn: aCanvas [
	"Draw the receiver on a canvas.
	Draw keyboard focus if appropriate."

	| fauxBounds |
	self setDefaultContentsIfNil.
	super drawOn: aCanvas.  "Border and background if any"
	false ifTrue: [self debugDrawLineRectsOn: aCanvas].  "show line rects for debugging"
	(self startingIndex > text size)
		ifTrue: [self drawNullTextOn: aCanvas].
	"Hack here:  The canvas expects bounds to carry the location of the text, but we also need to communicate clipping."
	fauxBounds := self bounds topLeft corner: self innerBounds bottomRight.
	aCanvas paragraph: self paragraph bounds: fauxBounds color: color.
]

{ #category : #accessing }
TextMorph >> editor [
	"Return my current editor, or install a new one."
	editor ifNotNil: [^ editor].
	^ self installEditorToReplace: nil
]

{ #category : #private }
TextMorph >> editorClass [
	"Answer the class used to create the receiver's editor"
	
	^SmalltalkEditor
]

{ #category : #accessing }
TextMorph >> elementCount [
	"Answer how many sub-objects are within me"

	^ self text string size 
]

{ #category : #testing }
TextMorph >> enabled [

	^super enabled and: [ self isLocked not ]
]

{ #category : #accessing }
TextMorph >> enabled: aBoolean [

	aBoolean 
		ifTrue: [ text makeAllColor: self defaultColor ]
		ifFalse: [ text makeAllColor: self theme disabledTextColor ].
		
	self changed
]

{ #category : #blinking }
TextMorph >> ensureCursor [
	paragraph ifNotNil: [ :p | p showCaret: true ]
]

{ #category : #editing }
TextMorph >> enterClickableRegion: evt [
	| |
	
	evt hand hasSubmorphs ifTrue:[^self].
	evt hand temporaryCursor ifNotNil:[^self].
	"paragraph ifNotNil:[
		(paragraph characterBlockAtPoint: evt position) stringIndex ].
"
]

{ #category : #'event handling' }
TextMorph >> escapePressed [
	
]

{ #category : #geometry }
TextMorph >> extent: aPoint [ 
	| newExtent priorEditor |
	bounds extent = aPoint ifTrue: [^ self].
	priorEditor := editor.
	self isAutoFit
		ifTrue: [wrapFlag ifFalse: [^ self].  "full autofit can't change"
				newExtent := aPoint truncated max: self minimumExtent.
				newExtent x = self extent x ifTrue: [^ self].  "No change of wrap width"
				self releaseParagraphReally.  "invalidate the paragraph cache"
				super extent: newExtent.
				priorEditor
					ifNil: [self fit]  "since the width has changed..."
					ifNotNil: [self installEditorToReplace: priorEditor]]
		ifFalse: [super extent: (aPoint truncated max: self minimumExtent).
				wrapFlag ifFalse: [^ self].  "no effect on composition"
				self composeToBounds]

]

{ #category : #'visual properties' }
TextMorph >> fillStyle [
	"Return the current fillStyle of the receiver."
	
	self assureExtension.
	^extension fillStyle ifNil: [
		backgroundColor
			ifNil: [Color transparent]]
]

{ #category : #'visual properties' }
TextMorph >> fillStyle: aFillStyle [
	"Set the current fillStyle of the receiver."
	
	backgroundColor := aFillStyle asColor.
	super fillStyle: aFillStyle
]

{ #category : #containment }
TextMorph >> fillingOnOff [
	"Establish a container for this text, with opposite filling status"
	self fillsOwner: (self fillsOwner not)
]

{ #category : #containment }
TextMorph >> fillsOwner [
	"Answer true if I fill my owner's shape."
	^container notNil and: [container fillsOwner]
]

{ #category : #containment }
TextMorph >> fillsOwner: aBoolean [ 
	self fillsOwner == aBoolean
		ifTrue: [^ self].
	self
		setContainer: (aBoolean
				ifTrue: [wrapFlag := true.
					container
						ifNil: [TextContainer new for: self minWidth: textStyle lineGrid * 2]
						ifNotNil: [container fillsOwner: true]]
				ifFalse: [self avoidsOcclusions
						ifFalse: [ nil ]
						ifTrue: [container fillsOwner: false]])
]

{ #category : #'linked frames' }
TextMorph >> firstCharacterIndex [
	^ self paragraph firstCharacterIndex
]

{ #category : #'linked frames' }
TextMorph >> firstInChain [
	"Return the first morph in a chain of textMorphs"

	| first |
	first := self.
	[first predecessor isNil] whileFalse: [first := first predecessor].
	^first
]

{ #category : #private }
TextMorph >> fit [
	"Adjust my bounds to fit the text.  Should be a no-op if autoFit is not specified.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent para cBounds lastOfLines heightOfLast |
	self isAutoFit 
		ifTrue: 
			[newExtent := (self paragraph extent max: 9 @ textStyle lineGrid) + (0 @ 2).
			newExtent := newExtent + (2 * borderWidth).
			newExtent := ((0 @ 0 extent: newExtent) expandBy: margins) extent.
			newExtent ~= bounds extent 
				ifTrue: 
					[(container isNil and: [successor isNil]) 
						ifTrue: 
							[para := paragraph.	"Save para (layoutChanged smashes it)"
							super extent: newExtent.
							paragraph := para]].
			(container notNil and: [successor isNil])
				ifTrue: 
					[cBounds := container bounds truncated.
					"23 sept 2000 - try to allow vertical growth"
					lastOfLines := self paragraph lines last.
					heightOfLast := lastOfLines bottom - lastOfLines top.
					(lastOfLines last < text size 
						and: [lastOfLines bottom + heightOfLast >= self bottom]) 
							ifTrue: 
								[container releaseCachedState.
								cBounds := cBounds origin corner: cBounds corner + (0 @ heightOfLast)].
					self privateBounds: cBounds]].

	"These statements should be pushed back into senders"
	self paragraph positionWhenComposed: self position.
	successor ifNotNil: [successor predecessorChanged].
	self changed	"Too conservative: only paragraph composition
					should cause invalidation."
]

{ #category : #accessing }
TextMorph >> font [
	"Answer the probable font"

	self textStyle fonts ifEmpty: [^TextStyle defaultFont].
	^self textStyle defaultFont.
		
]

{ #category : #accessing }
TextMorph >> font: aFont [
	| newTextStyle |
	newTextStyle := aFont textStyle copy ifNil: [ TextStyle fontArray: { aFont } ].
	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOf: aFont)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]
]

{ #category : #accessing }
TextMorph >> fontName: fontName pointSize: fontSize [
	| newTextStyle |
	newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy.
	newTextStyle ifNil: [self error: 'font ', fontName, ' not found.'].

	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfPointSize: fontSize)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]
]

{ #category : #accessing }
TextMorph >> fontName: fontName size: fontSize [
	| newTextStyle |
	newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy.
	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfSize: fontSize)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]
]

{ #category : #'scripting access' }
TextMorph >> getAllButFirstCharacter [
	"Obtain all but the first character from the receiver; if that would be empty, return a black dot"

	| aString |
	^ (aString := text string) size > 1 ifTrue: [aString copyFrom: 2 to: aString size] ifFalse: ['·']
]

{ #category : #accessing }
TextMorph >> getCharacters [
	"obtain a string value from the receiver"

	^ self text string copy
]

{ #category : #accessing }
TextMorph >> getFirstCharacter [
	"obtain the first character from the receiver if it is empty, return a  
	black dot"
	| aString |
	^ (aString := text string) isEmpty
		ifTrue: ['·']
		ifFalse: [aString first asString] 
]

{ #category : #accessing }
TextMorph >> getLastCharacter [
	"obtain the last character from the receiver if it is empty, return a black dot"

	| aString |
	^ (aString := text string) size > 0 ifTrue: [aString last asString] ifFalse: ['·']
]

{ #category : #'event handling' }
TextMorph >> getMenu: shiftKeyState [ 
	^ shiftKeyState not
		ifTrue: [TextEditor yellowButtonMenu]
		ifFalse: [TextEditor shiftedYellowButtonMenu]
]

{ #category : #'submorphs-add/remove' }
TextMorph >> goBehind [
	"We need to save the container, as it knows about fill and run-around"
	| cont |
	container ifNil: [^ super goBehind].
	self releaseParagraph.  "Cause recomposition"
	cont := container.  "Save the container"
	super goBehind.  "This will change owner, nilling the container"
	container := cont.  "Restore the container"
	self changed
]

{ #category : #editing }
TextMorph >> handleEdit: editBlock [
	"Ensure that changed areas get suitably redrawn"
	self selectionChanged.  "Note old selection"
		editBlock value.
	self selectionChanged.  "Note new selection"
	self updateFromParagraph  "Propagate changes as necessary"
]

{ #category : #editing }
TextMorph >> handleInteraction: interactionBlock [
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the paragraph.
	Couple ParagraphEditor to Morphic keyboard events"
	| oldEditor oldParagraph oldText |
	oldEditor := self editor.
	oldParagraph := paragraph.
	oldText := oldParagraph text copy.

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	(oldParagraph == paragraph) ifTrue:[
		"this will not work if the paragraph changed"
		editor := oldEditor.     "since it may have been changed while in block"
	].
	self selectionChanged.  "Note new selection"
	(oldText = paragraph text and: [ oldText runs = paragraph text runs ])
		ifFalse:[ self updateFromParagraph ].
]

{ #category : #'event handling' }
TextMorph >> handleKeystroke: anEvent [ 
	"System level event handling."

	anEvent wasHandled
		ifTrue: [^ self].
	self allowsKeymapping
		ifTrue: [ self dispatchKeystrokeForEvent: anEvent ] .
	anEvent wasHandled
		ifTrue: [
			"We need to check for completion here because otherwise it will try to handle the keystroke, 
			even if it was already processed."
			(Smalltalk tools hasToolNamed: #codeCompletion) 
				ifTrue: [ Smalltalk tools codeCompletion uniqueInstance closeMenu ].
			^ self].
	(self handlesKeyStroke: anEvent)
		ifFalse: [^ self].
	self keyStroke: anEvent.
	anEvent wasHandled: true.
]

{ #category : #'events-processing' }
TextMorph >> handleMouseMove: anEvent [
	"Re-implemented to allow for mouse-up move events"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent hand hasSubmorphs) ifTrue:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse:[^self].
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: 1].

]

{ #category : #'event handling' }
TextMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #'event handling' }
TextMorph >> handlesMouseDown: evt [
	^ self innerBounds containsPoint: evt cursorPoint
]

{ #category : #'event handling' }
TextMorph >> handlesMouseOver: evt [ 
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?"
	^ self enabled
]

{ #category : #'event handling' }
TextMorph >> hasFocus [
	^editor notNil
]

{ #category : #editing }
TextMorph >> hasUnacceptedEdits: aBoolean [
	"Ignored here, but noted in TextMorphForEditView"

]

{ #category : #'event handling' }
TextMorph >> hideOverEditableTextCursor [  
	self currentHand showTemporaryCursor: nil
	
]

{ #category : #initialization }
TextMorph >> initialize [
	super initialize.
	borderWidth := 0.
	textStyle := TextStyle default copy.
	wrapFlag := true.
	margins := Margin left: 0 right: 0 top: 0 bottom: 0.
	
	self attachKeymapCategory: #TextMorph.
]

{ #category : #'scripting access' }
TextMorph >> insertCharacters: aSource [
	"Insert the characters from the given source at my current cursor position"

	| aLoc |
	aLoc := self cursor max: 1.
	paragraph replaceFrom: aLoc to: (aLoc - 1) with: aSource asText displaying: true.
	self updateFromParagraph  
]

{ #category : #private }
TextMorph >> installEditorToReplace: priorEditor [
	"Install an editor for my paragraph.  This constitutes 'hasFocus'.
	If priorEditor is not nil, then initialize the new editor from its state.
	We may want to rework this so it actually uses the prior editor."

	| stateArray |
	priorEditor ifNotNil: [stateArray := priorEditor stateArray].
	editor := self editorClass forMorph: self.
	priorEditor ifNotNil: [editor stateArrayPut: stateArray].
	self selectionChanged.
	^ editor
]

{ #category : #accessing }
TextMorph >> isAutoFit [
	^ self valueOfProperty: #autoFitContents ifAbsent: [true]

]

{ #category : #'linked frames' }
TextMorph >> isLinkedTo: aMorph [
	self firstInChain withSuccessorsDo:
		[:m | m == aMorph ifTrue: [^ true]].
	^ false
]

{ #category : #classification }
TextMorph >> isTextMorph [
	^true
]

{ #category : #accessing }
TextMorph >> isTranslucentButNotTransparent [
	"Overridden from BorderedMorph to test backgroundColor instead of (text) color."

	backgroundColor ifNil: [ ^ true ].
	(backgroundColor isColor and: [ backgroundColor isTranslucentButNotTransparent ])
		ifTrue: [ ^ true ].
	(borderColor isColor and: [ borderColor isTranslucentButNotTransparent ])
		ifTrue: [ ^ true ].
	^ false
]

{ #category : #accessing }
TextMorph >> isWrapped [
	
	^wrapFlag
]

{ #category : #'editing helpers' }
TextMorph >> italic [
	
	self changeEmphasis: #italic
]

{ #category : #alignment }
TextMorph >> justified [ 
	self changeAlignment: #justified
]

{ #category : #'event handling' }
TextMorph >> keyStroke: evt [ 
	"Handle a keystroke event."

	(Smalltalk tools hasToolNamed: #codeCompletion) 
		ifTrue: [ 
			Smalltalk tools 
				codeCompletionAround: [ self basicKeyStroke: evt ] 
				textMorph: self 
				keyStroke: evt ]
		ifFalse: [  self basicKeyStroke: evt ]
]

{ #category : #'event handling' }
TextMorph >> keyboardFocusChange: aBoolean [ 	
	super keyboardFocusChange: aBoolean.
	paragraph isNil ifFalse:[paragraph focused: aBoolean].	
	aBoolean 
		ifTrue:["A hand is wanting to send us characters..."
			self hasFocus ifFalse: [self editor	"Forces install"].
			self showOverEditableTextCursor]
		ifFalse:["A hand has clicked elsewhere..."		
			self world 
				ifNotNil:[:w | w handsDo: [:h | h keyboardFocus == self ifTrue: [^self]].
					"Release control unless some hand is still holding on"			
					self releaseEditor].
			self hideOverEditableTextCursor].
	self manageCursor.
	self focusChanged
]

{ #category : #'linked frames' }
TextMorph >> lastCharacterIndex [
	^ self paragraph lastCharacterIndex
]

{ #category : #alignment }
TextMorph >> leftFlush [ 

	self changeAlignment: #leftFlush

]

{ #category : #blinking }
TextMorph >> manageCursor [
	(paragraph isNil or: [paragraph focused not])
		ifTrue: [^ self
				resetBlinkCursor;
				stopBlinking].
	Editor blinkingCursor 
		ifTrue: [self startBlinking]
		ifFalse: [self ensureCursor]
]

{ #category : #accessing }
TextMorph >> margins [

	^margins
]

{ #category : #accessing }
TextMorph >> margins: newMargins [
	"newMargins can be a number, point , as allowed by, eg, insetBy:."

	margins := newMargins asMargin.
	self composeToBounds
]

{ #category : #geometry }
TextMorph >> minimumExtent [


	| minExt |
	textStyle ifNil: [^ 9@16].
	borderWidth ifNil: [^ 9@16].
	minExt := (9@(textStyle lineGrid+2)) + (borderWidth*2).
	^ ((0@0 extent: minExt) expandBy: margins) extent
]

{ #category : #'event handling' }
TextMorph >> mouseDown: evt [ 
	"Make this TextMorph be the keyboard input focus, if it isn't  
	already, and repond to the text selection gesture"
	
	evt yellowButtonPressed
		ifTrue: ["First check for option (menu) click"
			(self yellowButtonActivity: evt shiftPressed) 
				ifTrue: [ ^ self ]].
	self handleInteraction: [editor mouseDown: evt].
	self hasKeyboardFocus
		ifFalse: [self takeKeyboardFocus].
	super mouseDown: evt
]

{ #category : #'event handling' }
TextMorph >> mouseEnter: evt [
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."
	super mouseEnter: evt.
	self showOverEditableTextCursor
	
]

{ #category : #'event handling' }
TextMorph >> mouseLeave: evt [
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
	super mouseLeave: evt.
	self hideOverEditableTextCursor
	
]

{ #category : #'event handling' }
TextMorph >> mouseMove: event [

	event redButtonPressed ifFalse: [ ^ self ].
	self handleInteraction: [ editor mouseMove: event ]
]

{ #category : #accessing }
TextMorph >> mouseUp: evt [
	self handleInteraction: [editor mouseUp: evt].
	(self boundsInWorld containsPoint: self currentHand position)
		ifFalse: [self hideOverEditableTextCursor]
]

{ #category : #'editing helpers' }
TextMorph >> narrow [
	
	self changeEmphasis: #narrow
]

{ #category : #accessing }
TextMorph >> newContents: stringOrText [ 
	"Accept new text contents."
	| newText embeddedMorphs |
	"If my text is all the same font, use the font for my new contents"
	newText := stringOrText isString ifTrue: [ | textSize |
		(text notNil
		  and: [ (textSize := text size) > 0
		    and: [ (text runLengthFor: 1) = textSize ]]) ifTrue: [ | attribs |
			attribs := text attributesAt: 1 forStyle: textStyle.
			Text string: stringOrText copy attributes: attribs.
		]
		ifFalse: [ Text fromString: stringOrText copy ]
	]
	ifFalse: [ stringOrText copy asText.	"should be veryDeepCopy?" ].

	(text = newText and: [text runs = newText runs]) ifTrue: [^ self].	"No substantive change"
	text ifNotNil: [(embeddedMorphs := text embeddedMorphs)
			ifNotNil: 
				[self removeAllMorphsIn: embeddedMorphs.
				embeddedMorphs do: [:m | m delete]]].

	text := newText.

	"add all morphs off the visible region; they'll be moved into the right 
	place when they become visible. (this can make the scrollable area too 
	large, though)"
	newText embeddedMorphs do: 
		[:m | 
		self addMorph: m.
		m position: -1000 @ 0].
	self releaseParagraph.
	"update the paragraph cache"
	self paragraph.
	"re-instantiate to set bounds"
	self world ifNotNil: [self world startSteppingSubmorphsOf: self]
]

{ #category : #'interactive error protocol' }
TextMorph >> nextTokenFrom: start direction: dir [
	^ self editor nextTokenFrom: start direction: dir
]

{ #category : #'editing helpers' }
TextMorph >> normal [
	
	self changeEmphasis: #normal
]

{ #category : #'interactive error protocol' }
TextMorph >> notify: aString at: anInteger in: aStream [
	^ self editor notify: aString at: anInteger in: aStream
]

{ #category : #containment }
TextMorph >> occlusionsOnOff [
	"Establish a container for this text, with opposite occlusion avoidance status"
	self setContainer:
	(container
	ifNil: [(TextContainer new for: self minWidth: textStyle lineGrid*2)
							fillsOwner: false; avoidsOcclusions: true]
	ifNotNil: [(container avoidsOcclusions and: [container fillsOwner not])
			ifTrue: [nil  "Return to simple rectangular bounds"]
			ifFalse: [container avoidsOcclusions: container avoidsOcclusions not]])
]

{ #category : #blinking }
TextMorph >> onBlinkCursor [
	"Blink the cursor"
	| para |
	para := self paragraph ifNil:[^nil].
	Time millisecondClockValue < self blinkStart ifTrue:[
		"don't blink yet"
		^para showCaret: para focused.
	].
	para showCaret: para showCaret not.
	para caretRect ifNotNil:[:r| self invalidRect: r].
]

{ #category : #'find-replace' }
TextMorph >> openFindDialog [
	self flash
]

{ #category : #'change reporting' }
TextMorph >> ownerChanged [
	| priorEditor |
	super ownerChanged.
	container ifNotNil: 
			[editor isNil 
				ifTrue:
					[self releaseParagraph.
					(container isKindOf: TextContainer) ifTrue:
						["May need to recompose due to changes in owner"
						self installEditorToReplace: nil.
						self releaseParagraph]]
				ifFalse: 
					[priorEditor := editor.
					self releaseParagraph.
					self installEditorToReplace: priorEditor]]
]

{ #category : #private }
TextMorph >> paragraph [
	"Paragraph instantiation is lazy -- create it only when needed"
	|newParagraph|
	paragraph ifNotNil: [^ paragraph].

	self setProperty: #CreatingParagraph toValue: true.
	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	newParagraph  := (Paragraph new textOwner: self owner).
	
	newParagraph wantsColumnBreaks: successor notNil.
	newParagraph
		compose: text
		style: textStyle copy
		from: self startingIndex
		in: self container.
	wrapFlag ifFalse:
		["Was given huge container at first... now adjust"
		newParagraph adjustRightX].
	newParagraph focused: (self currentHand keyboardFocus == self).
	paragraph := newParagraph.
	self fit.
	self removeProperty: #CreatingParagraph.


	^ paragraph
]

{ #category : #editing }
TextMorph >> passKeyboardFocusTo: otherMorph [ 
	| w |
	self flag: #arNote.	"Do we need this?!"
	(w := self world) isNil 
		ifFalse: 
			[w 
				handsDo: [:h | h keyboardFocus == self ifTrue: [h newKeyboardFocus: otherMorph]]]
]

{ #category : #'linked frames' }
TextMorph >> predecessor [
	^ predecessor
]

{ #category : #private }
TextMorph >> predecessor: pred successor: succ [
	"Private -- for use only in morphic duplication"
	predecessor := pred.
	successor := succ.

]

{ #category : #private }
TextMorph >> predecessorChanged [
	| newStart oldStart |
	(self hasProperty: #CreatingParagraph) ifTrue: [^self].
	newStart := predecessor isNil 
				ifTrue: [1]
				ifFalse: [predecessor lastCharacterIndex + 1].
	(self paragraph adjustedFirstCharacterIndex ~= newStart 
		or: [newStart >= text size]) 
			ifTrue: 
				[paragraph composeAllStartingAt: newStart.
				self fit]
			ifFalse: 
				["If the offset to end of text has not changed, just slide"

				oldStart := self firstCharacterIndex.
				self withSuccessorsDo: [:m | m adjustLineIndicesBy: newStart - oldStart]]
]

{ #category : #editing }
TextMorph >> preferredKeyboardPosition [

	| default rects |
	default  := (self bounds: self bounds in: World) topLeft.
	paragraph ifNil: [^ default].
	rects := paragraph selectionRects.
	rects size = 0 ifTrue: [^ default].
	^ rects first topLeft.

	"^ (self bounds: self bounds in: World) topLeft."

]

{ #category : #geometry }
TextMorph >> privateMoveBy: delta [ 
	super privateMoveBy: delta.
	editor ifNil: [paragraph ifNotNil: [paragraph moveBy: delta]]
		ifNotNil: 
			["When moving text with an active editor, save and restore all state."

			paragraph moveBy: delta.
			self installEditorToReplace: editor]
]

{ #category : #private }
TextMorph >> privateOwner: newOwner [
	"Nil the container when text gets extracted"
	super privateOwner: newOwner.
	container ifNotNil: [
		newOwner ifNotNil: [
			newOwner isWorldOrHandMorph ifTrue: [self setContainer: nil]]]
]

{ #category : #'linked frames' }
TextMorph >> recomposeChain [
	"Recompose this textMorph and all that follow it."
	self withSuccessorsDo:
		[:m |  m text: text textStyle: textStyle;  "Propagate new style if any"
				releaseParagraph;  "Force recomposition"
				fit  "and propagate the change"]
]

{ #category : #caching }
TextMorph >> releaseCachedState [
 
	super releaseCachedState.
	self releaseParagraph

]

{ #category : #private }
TextMorph >> releaseEditor [ 
	"Release the editor for my paragraph.  This morph no longer 'hasFocus'."
	editor ifNotNil:
		[self selectionChanged.
		self paragraph selectionStart: nil selectionStop: nil.
		editor := nil].
]

{ #category : #private }
TextMorph >> releaseParagraph [

	"a slight kludge so subclasses can have a bit more control over whether the paragraph really 
	gets released. important for GeeMail since the selection needs to be accessible even if the 
	hand is outside me"

	self releaseParagraphReally.

]

{ #category : #private }
TextMorph >> releaseParagraphReally [

	"a slight kludge so subclasses can have a bit more control over whether the paragraph really 
	gets released. important for GeeMail since the selection needs to be accessible even if the 
	hand is outside me"

	"Paragraph instantiation is lazy -- it will be created only when needed"
	self releaseEditor.
	paragraph ifNotNil:
		[paragraph := nil].
	container ifNotNil:
		[container releaseCachedState]
]

{ #category : #private }
TextMorph >> removedMorph: aMorph [
	| range |
	range := text find: (TextAnchor new anchoredMorph: aMorph).
	range ifNotNil:
		[self paragraph replaceFrom: range first to: range last
				with: Text new displaying: false.
		self fit].
	aMorph textAnchorType: nil.
	aMorph relativeTextAnchorPosition: nil.
	super removedMorph: aMorph.
]

{ #category : #blinking }
TextMorph >> resetBlinkCursor [
	"Reset the blinking cursor"
	| para |
	self blinkStart: Time millisecondClockValue + 500.
	para := self paragraph ifNil:[^self].
	para showCaret = para focused ifFalse:[
		para caretRect ifNotNil:[:r| self invalidRect: r].
		para showCaret: para focused.
	].

]

{ #category : #menu }
TextMorph >> reverseCurveDirection [
	container textDirection: container textDirection negated.
	self paragraph composeAll
]

{ #category : #alignment }
TextMorph >> rightFlush [ 
	
	self changeAlignment: #rightFlush

]

{ #category : #accessing }
TextMorph >> select [
	editor ifNotNil: [ editor select ]
]

{ #category : #accessing }
TextMorph >> selectAll [
	self editor selectFrom: 1 to: text size
]

{ #category : #accessing }
TextMorph >> selectFrom: a to: b [
	self editor selectFrom: a to: b
]

{ #category : #accessing }
TextMorph >> selectInvisiblyFrom: start to: stop [
	editor ifNotNil: [ editor selectInvisiblyFrom: start to: stop ]
]

{ #category : #accessing }
TextMorph >> selection [
	^editor ifNotNil: [ editor selection ]
]

{ #category : #private }
TextMorph >> selectionChanged [
	"Invalidate all the selection rectangles. 
	Make sure that any drop shadow is accounted for too."
	self paragraph selectionRects
		do: [:r | | intr |
			intr := r intersect: self fullBounds ifNone: [ nil ].
			intr ifNotNil: [ self invalidRect: (self expandFullBoundsForDropShadow: intr)] ]
]

{ #category : #accessing }
TextMorph >> selectionInterval [
	^editor ifNotNil: [ editor selectionInterval ]
]

{ #category : #accessing }
TextMorph >> setCharacters: chars [
	"obtain a string value from the receiver"

	(self getCharacters = chars) ifFalse:
		[self newContents: chars]
]

{ #category : #containment }
TextMorph >> setContainer: newContainer [
	"Adopt (or abandon) container shape"
	self changed.
	container := newContainer.
	self releaseParagraph
]

{ #category : #menu }
TextMorph >> setCurveBaseline: evt [
	| handle origin |
	origin := evt cursorPoint.
	handle := HandleMorph new forEachPointDo:
		[:newPoint | handle removeAllMorphs.
		handle addMorph:
			(PolygonMorph vertices: (Array with: origin with: newPoint)
				color: Color black borderWidth: 1 borderColor: Color black).
		container baseline: (newPoint - origin) y negated asInteger // 5.
		self paragraph composeAll].
	evt hand attachMorph: handle.
	handle startStepping	
]

{ #category : #private }
TextMorph >> setDefaultContentsIfNil [
	"Set the default contents"

	| toUse |
	text ifNil:
		[toUse := self valueOfProperty: #defaultContents.
		toUse ifNil: [toUse :='' asText "allBold"].	"try it plain for a while"
		text := toUse]
]

{ #category : #accessing }
TextMorph >> setFirstCharacter: source [ 
	"Set the first character of the receiver as indicated"
	| aChar chars |
	aChar := source asCharacter.
	(chars := self getCharacters) isEmpty
		ifTrue: [self
				newContents: (String with: aChar)]
		ifFalse: [chars first = aChar
				ifFalse: [self
						newContents: (String
								streamContents: [:aStream | 
									aStream nextPut: aChar.
									aStream
										nextPutAll: (chars copyFrom: 2 to: chars size)])]] 
]

{ #category : #accessing }
TextMorph >> setLastCharacter: source [
	"Set the last character of the receiver as indicated"

	| aChar chars |
	aChar := source asCharacter.
	(chars := self getCharacters) size > 0 
		ifFalse:
			[self newContents: (String with: aChar)]
		ifTrue:
			[(chars last = aChar) ifFalse:
				[self newContents: (String streamContents:
					[:aStream |
						aStream nextPutAll: (chars copyFrom: 1 to: (chars size - 1)).
						aStream nextPut: aChar])]]
]

{ #category : #private }
TextMorph >> setPredecessor: newPredecessor [
	predecessor := newPredecessor
]

{ #category : #private }
TextMorph >> setSuccessor: newSuccessor [

	successor := newSuccessor.
	paragraph ifNotNil: [paragraph wantsColumnBreaks: successor notNil].

]

{ #category : #initialization }
TextMorph >> setTextStyle: aTextStyle [

	textStyle := aTextStyle.
	self releaseCachedState; changed
]

{ #category : #'find-replace' }
TextMorph >> sharesFindReplace [
	^ false
]

{ #category : #menu }
TextMorph >> shiftedYellowButtonActivity [
	"Supply the normal 'code pane' menu to use its text editing commands from a menu."

	self editor pluggableYellowButtonActivity: true.
	self changed.

]

{ #category : #'event handling' }
TextMorph >> showOverEditableTextCursor [
	| o |
	owner isNil
		ifTrue: [^ self].
	o := owner isWorldMorph 
		ifTrue: [self] 
		ifFalse: [owner].
	(o boundsInWorld containsPoint: self currentHand position)
		ifTrue: [self currentHand showTemporaryCursor: (self theme overTextCursorFor: self)]

]

{ #category : #blinking }
TextMorph >> startBlinking [
	self startStepping: #onBlinkCursor 
		at: Time millisecondClockValue 
		arguments: nil stepTime: 500.
	self resetBlinkCursor.

]

{ #category : #'linked frames' }
TextMorph >> startingIndex [
	predecessor isNil
		ifTrue: [^ 1].
	^ predecessor lastCharacterIndex + 1 
]

{ #category : #blinking }
TextMorph >> stopBlinking [
	self stopSteppingSelector: #onBlinkCursor.

]

{ #category : #initialization }
TextMorph >> string: aString fontName: aName size: aSize [

	self string: aString fontName: aName size: aSize wrap: true
]

{ #category : #initialization }
TextMorph >> string: aString fontName: aName size: aSize wrap: shouldWrap [

	shouldWrap
		ifTrue: [self contentsWrapped: aString]
		ifFalse: [self contents: aString].
	self fontName: aName size: aSize
]

{ #category : #'editing helpers' }
TextMorph >> struckOut [
	
	self changeEmphasis: #struckOut
]

{ #category : #'linked frames' }
TextMorph >> successor [
	^ successor
]

{ #category : #accessing }
TextMorph >> text [
	^ text
]

{ #category : #private }
TextMorph >> text: t textStyle: s [
	"Private -- for use only in morphic duplication"
	text := t.
	defaultColor := (t attributesAt: 1) detect: [ :e | e isKindOf: TextColor ] ifNone: [ self color ].
	textStyle := s.
	paragraph ifNotNil: [paragraph textStyle: s]
]

{ #category : #private }
TextMorph >> text: t textStyle: s wrap: wrap color: c
	predecessor: pred successor: succ [
	"Private -- for use only in morphic duplication"
	text := t.
	defaultColor := (t attributesAt: 1) detect: [ :e | e isKindOf: TextColor ].
	textStyle := s.
	wrapFlag := wrap.
	color := c.
	paragraph := editor := container := nil.
	self predecessor: pred successor: succ
]

{ #category : #accessing }
TextMorph >> textAlignment [
	"Answer 1..4, representing #leftFlush, #rightFlush, #centered, or #justified"
	^self editor textAlignment
]

{ #category : #accessing }
TextMorph >> textAlignmentSymbol [
	"Answer one of #leftFlush, #rightFlush, #centered, or #justified"
	^self editor textAlignmentSymbol
]

{ #category : #geometry }
TextMorph >> textBounds [
	^ bounds
]

{ #category : #accessing }
TextMorph >> textColor [

	^ color
]

{ #category : #accessing }
TextMorph >> textColor: aColor [

	color = aColor ifTrue: [^ self].
	color := aColor.
	text addAttribute: (TextColor color: aColor).
	self changed.

]

{ #category : #accessing }
TextMorph >> textStyle [
	^textStyle
]

{ #category : #'editing helpers' }
TextMorph >> underlined [
	
	self changeEmphasis: #underlined
]

{ #category : #private }
TextMorph >> updateFromParagraph [
	"A change has taken place in my paragraph, as a result of editing and I must be updated.  If a line break causes recomposition of the current paragraph, or it the selection has entered a different paragraph, then the current editor will be released, and must be reinstalled with the resulting new paragraph, while retaining any editor state, such as selection, undo state, and current typing emphasis."

	| newStyle sel oldLast oldEditor back |
	paragraph ifNil: [^self].
	wrapFlag ifNil: [wrapFlag := true].
	editor ifNotNil: 
			[oldEditor := editor.
			sel := editor selectionInterval.
			editor storeSelectionInParagraph].
	text := paragraph text.
	paragraph textStyle = textStyle 
		ifTrue: [self fit]
		ifFalse: 
			["Broadcast style changes to all morphs"

			newStyle := paragraph textStyle.
			(self firstInChain text: text textStyle: newStyle) recomposeChain.
			editor ifNotNil: [self installEditorToReplace: editor]].
	super layoutChanged.
	sel ifNil: [^self].

	"If selection is in top line, then recompose predecessor for possible ripple-back"
	predecessor ifNotNil: 
			[sel first <= (self paragraph lines first last + 1) 
				ifTrue: 
					[oldLast := predecessor lastCharacterIndex.
					predecessor paragraph 
						recomposeFrom: oldLast
						to: text size
						delta: 0.
					oldLast = predecessor lastCharacterIndex 
						ifFalse: 
							[predecessor changed.	"really only last line"
							self predecessorChanged]]].
	((back := predecessor notNil 
				and: [sel first <= self paragraph firstCharacterIndex]) or: 
				[successor notNil 
					and: [sel first > (self paragraph lastCharacterIndex + 1)]]) 
		ifTrue: 
			["The selection is no longer inside this paragraph.
		Pass focus to the paragraph that should be in control."

			back ifTrue: [predecessor recomposeChain] ifFalse: [self recomposeChain].
			self firstInChain withSuccessorsDo: 
					[:m | 
					(sel first between: m firstCharacterIndex and: m lastCharacterIndex + 1) 
						ifTrue: 
							[m installEditorToReplace: oldEditor.
							^self passKeyboardFocusTo: m]].
			self error: 'Inconsistency in text editor'	"Must be somewhere in the successor chain"].
	editor ifNil: 
			["Reinstate selection after, eg, style change"

			self installEditorToReplace: oldEditor].
	"self setCompositionWindow."

]

{ #category : #accessing }
TextMorph >> userString [
	"Do I have a text string to be searched on?"

	^ text string
]

{ #category : #copying }
TextMorph >> veryDeepFixupWith: deepCopier [ 
	"If target and arguments fields were weakly copied, fix them here.  If 
	they were in the tree being copied, fix them up, otherwise point to the 
	originals!"

	super veryDeepFixupWith: deepCopier.
	"It makes no sense to share pointers to an existing predecessor and successor"
	predecessor := deepCopier references at: predecessor ifAbsent: [nil].
	successor := deepCopier references at: successor ifAbsent: [nil]
]

{ #category : #copying }
TextMorph >> veryDeepInner: deepCopier [ 
	"Copy all of my instance variables. Some need to be not copied at all, but shared.
	Warning!! Every instance variable defined in this class must be handled.
	We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	textStyle := textStyle veryDeepCopyWith: deepCopier.
	text := text veryDeepCopyWith: deepCopier.
	wrapFlag := wrapFlag veryDeepCopyWith: deepCopier.
	paragraph := paragraph veryDeepCopyWith: deepCopier.
	editor := editor veryDeepCopyWith: deepCopier.
	container := container veryDeepCopyWith: deepCopier.
	predecessor := predecessor.
	successor := successor.
	backgroundColor := backgroundColor veryDeepCopyWith: deepCopier.
	margins := margins veryDeepCopyWith: deepCopier.

]

{ #category : #'linked frames' }
TextMorph >> withSuccessorsDo: aBlock [ 
	"Evaluate aBlock for each morph in my successor chain"

	| each |
	each := self.
	[each isNil] whileFalse: 
			[aBlock value: each.
			each := each successor]
]

{ #category : #'event handling' }
TextMorph >> wouldAcceptKeyboardFocusUponTab [
	"Answer whether the receiver might accept keyboard focus if 
	tab were hit in some container playfield"
	^ true
]

{ #category : #accessing }
TextMorph >> wrapFlag [
	^ wrapFlag 
	
]

{ #category : #accessing }
TextMorph >> wrapFlag: aBoolean [
	"Should contained text stay wrapped at my width?"

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag := aBoolean.
	self composeToBounds
]

{ #category : #menu }
TextMorph >> wrapOnOff [
	self wrapFlag: wrapFlag not
]

{ #category : #menu }
TextMorph >> wrapString [
	"Answer the string to put in a menu that will invite the user to 
	switch autoFit mode"
	^ (wrapFlag
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text wrap to bounds' translated
]

{ #category : #'event handling' }
TextMorph >> yellowButtonActivity [
	"Supply the normal 'code pane' menu to use its text editing 
	commands from a menu."
	^ self yellowButtonActivity:false
]

{ #category : #'event handling' }
TextMorph >> yellowButtonActivity: shiftKeyState [ 
	"Invoke the text-editing menu.
	Check if required first!"
	
	self wantsYellowButtonMenu
		ifFalse: [ ^ false ].
		
	(self getMenu: shiftKeyState)
		ifNotNil: [ :menu|
			menu setInvokingView: self editor.
			menu invokeModal. self changed.
			^ true].
		
	^ true
]
