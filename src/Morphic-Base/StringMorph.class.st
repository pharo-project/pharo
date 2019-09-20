"
StringMorph is a ""lightweight"" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back TextStyle 
				or nil			defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 								italics, narrow, struckout) 
contents 		String 			The text that will be displayed. 
hasFocus 		Boolean 		Do I have the keyboard focus or not? 

A StringMorph may also be used like a SimpleButtonMorph to do an action when clicked. Use the menu 'extras' / 'add mouseUpAction'.

The following propery will be defined:
aStringMorph valueOfProperty: #mouseUpCodeToRun
"
Class {
	#name : #StringMorph,
	#superclass : #Morph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'font',
		'emphasis',
		'contents',
		'hasFocus',
		'backgroundColor'
	],
	#classVars : [
		'EditableStringMorph'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #'instance creation' }
StringMorph class >> contents: aString [
	" 'StringMorph contents: str' is faster than 'StringMorph new contents: str' "
	^ self contents: aString font: nil
]

{ #category : #'instance creation' }
StringMorph class >> contents: aString font: aFont [
	^ self contents: aString font: aFont emphasis: 0
]

{ #category : #'instance creation' }
StringMorph class >> contents: aString font: aFont emphasis: aCodeOrTextEmphasis [
	^ self new initWithContents: aString font: aFont emphasis: aCodeOrTextEmphasis value
]

{ #category : #setting }
StringMorph class >> editableStringMorph [
	^ EditableStringMorph ifNil: [EditableStringMorph := false]
]

{ #category : #setting }
StringMorph class >> editableStringMorph: aBoolean [
	EditableStringMorph := aBoolean
]

{ #category : #examples }
StringMorph class >> exampleBoldAndItalic [
	<sampleInstance>
	
	| font |
	font := LogicalFont familyName: StandardFonts defaultFont familyName pointSize: 42. 
	^ (self contents: 'This is a StringMorph with emphasis bold and italic' font: font emphasis: TextEmphasis bold)
			addEmphasis: TextEmphasis italic;
			position: 100@100;
			backgroundColor: Color orange;
			openInWorld
]

{ #category : #examples }
StringMorph class >> exampleFullEmphaisedString [
	<sampleInstance>
	| font |
	font := LogicalFont familyName: StandardFonts defaultFont familyName pointSize: 42. 
	^ (self contents: 'This is a StringMorph with emphasis 2r11111' font: font emphasis: 2r11111) 
			position: 100@100;
			backgroundColor: Color orange;
			openInWorld
]

{ #category : #examples }
StringMorph class >> exampleManyStringMorphs [
	"Return a morph with lots of strings for testing display speed."
	<sampleInstance>
	
	| c |
	c := AlignmentMorph newColumn.
	self class environment organization categories do:
		[:cat | c addMorph: (StringMorph new contents: cat)].
	^ c position: 100@50;
			openInWorld
]

{ #category : #editing }
StringMorph >> acceptContents [
	"The message is sent when the user hits enter or Cmd-S. Accept the current contents and end editing. This default implementation does nothing."

]

{ #category : #editing }
StringMorph >> acceptValue: aValue [
	| val |
	self contents: (val := aValue asString).
	^ val
]

{ #category : #menu }
StringMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' selector: #changeFont.
	aCustomMenu add: 'change emphasis' selector: #changeEmphasis.

]

{ #category : #accessing }
StringMorph >> addEmphasis: aCodeOrTextEmphasis [
	self emphasis: (emphasis bitOr: aCodeOrTextEmphasis value)
]

{ #category : #drawing }
StringMorph >> areasRemainingToFill: aRectangle [

	^ Array with: aRectangle
]

{ #category : #accessing }
StringMorph >> backgroundColor [
	
	^backgroundColor
]

{ #category : #accessing }
StringMorph >> backgroundColor: aColor [

	backgroundColor := aColor 
]

{ #category : #'halos and balloon help' }
StringMorph >> boundsForBalloon [
	"Some morphs have bounds that are way too big.  This is a contorted way of making things work okay in PluggableListMorphs, whose list elements historically have huge widths"

	| ownerOwner |
	^ ((owner notNil and: [(ownerOwner := owner owner) notNil]) and:
			[ownerOwner isKindOf: PluggableListMorph])
		ifTrue:
			[self boundsInWorld intersect: ownerOwner boundsInWorld ifNone: [self boundsInWorld] ]
		ifFalse:
			[super boundsForBalloon]
]

{ #category : #editing }
StringMorph >> cancelEdits [

	self doneWithEdits
]

{ #category : #menu }
StringMorph >> changeEmphasis [

	| reply |
	reply := UIManager default 
				chooseFrom: (self emphasisChoices collect: [:t | t translated]) 
				values: self emphasisChoices.
	reply ifNotNil: [ self emphasis: (TextEmphasis perform: reply) emphasisCode ].

]

{ #category : #accessing }
StringMorph >> contents [

	^ contents
]

{ #category : #accessing }
StringMorph >> contents: newContents [ 
	self privateSetContents: newContents.
	self fitContents
]

{ #category : #accessing }
StringMorph >> contentsClipped: aString [
	"Change my text, but do not change my size as a result"
	contents = aString ifTrue: [^ self].  "No substantive change"
	contents := aString.
	self changed
]

{ #category : #accessing }
StringMorph >> contrastingBackgroundColor [
	"sets the backgroundColor to either black or white, which ever makes the string simplest to read"
	backgroundColor := self color contrastingBlackAndWhiteColor 
]

{ #category : #initialization }
StringMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ self theme textColor
]

{ #category : #editing }
StringMorph >> doneWithEdits [

	hasFocus := false
]

{ #category : #drawing }
StringMorph >> drawDropShadowOn: aCanvas [

	| bnd gap |
	bnd := self bounds translateBy: self shadowOffset.
	gap := self layoutInset.
	bnd := (bnd topLeft + gap) corner: (bnd bottomRight + gap).
	aCanvas drawString: contents in: bnd font: self fontToUse color: self shadowColor.
]

{ #category : #drawing }
StringMorph >> drawOn: aCanvas [
	
	aCanvas
	
		fillRectangle: self bounds 
		fillStyle: self backgroundColor;
		
		drawString: self contents 
		in: self stringBounds 
		font: self fontToUse 
		color: self stringColor
		underline: (emphasis bitAnd: 2r100) ~~ 0
		underlineColor: self underlineColor 
		strikethrough: (emphasis bitAnd: 2r10000) ~~ 0
		strikethroughColor: self strikethroughColor
]

{ #category : #accessing }
StringMorph >> editorProvider [
	^ self valueOfProperty: #editorProvider ifAbsent: []
]

{ #category : #accessing }
StringMorph >> editorProvider: aValuable [
	self setProperty: #editorProvider toValue: aValuable
]

{ #category : #accessing }
StringMorph >> emphasis: aCodeOrTextEmphasis [
	"Set my emphasis to aCodeOrTextEmphasis, which is either a TextEmphasis object,
	or a numeric code that is treated as a bitmask.   In the case of a TextEmphasis,
	sending the message value will return the code number.  
	
	The bits have the following significance (see also the class comment for TextEmphasis)
	
	bit 			attribute
	2r1	 (1) 		bold 
	2r10 (2)	 	italic 
	2r100 (4) 	underlined 
	2r1000	 (8) 	narrow 
	2r10000	 (16) 	strikethrough 
			
	examples: 
		2r0 -> plain.  
		2r1 -> bold. 
		2r11 -> bold + italic.  
		2r1101 -> bold + underlined + strikethrough.
	etc...
	"

	emphasis := aCodeOrTextEmphasis value.
	^ self font: font emphasis: emphasis
]

{ #category : #menu }
StringMorph >> emphasisChoices [
	"Returns the emphasis selectors that are sent to a TextEmphasis."
	
	^ #(normal bold italic narrow underlined struckOut)
]

{ #category : #accessing }
StringMorph >> enabled: aBoolean [

	aBoolean 
		ifTrue: [ self color: self defaultColor ]
		ifFalse: [ self color: self theme disabledTextColor ]
]

{ #category : #accessing }
StringMorph >> fitContents [

	| newBounds boundsChanged |
	newBounds := self measureContents.
	boundsChanged := bounds extent ~= newBounds.
	self extent: newBounds.		"default short-circuits if bounds not changed"
	boundsChanged ifFalse: [self changed]
]

{ #category : #accessing }
StringMorph >> font [
	"who came up with #fontToUse rather than font?!"
	^self fontToUse
]

{ #category : #printing }
StringMorph >> font: aFont [ 
	"Set the font my text will use. The emphasis remains unchanged."

	font := aFont.
	^ self font: font emphasis: emphasis
]

{ #category : #accessing }
StringMorph >> font: aFont emphasis: emphasisCode [
	font := aFont.
	emphasis := emphasisCode.
	self fitContents.
"
in inspector say,
	 self font: (TextStyle default fontAt: 2) emphasis: 1
"
]

{ #category : #accessing }
StringMorph >> fontName: fontName size: fontSize [

	^ self font: (StrikeFont familyName: fontName size: fontSize) 
			emphasis: 0
]

{ #category : #accessing }
StringMorph >> fontToUse [

	| fontToUse |

	fontToUse := font ifNil: [ TextStyle defaultFont ] ifNotNil: [ font ].
	( emphasis isNil or: [ emphasis = 0 ] )
		ifTrue: [ ^ fontToUse ]
		ifFalse: [ ^ fontToUse emphasized: emphasis ]
]

{ #category : #layout }
StringMorph >> fullBounds [
	self contents ifNil: [ self contents: 'String Morph' ].
	^super fullBounds
]

{ #category : #'event handling' }
StringMorph >> handlesMouseDown: evt [
	"If the shift key is pressed then yes.
	As normal if StringMorph class>> editableStringMorph  returns false."
	
	^ (self isEditable: evt)
		ifTrue: [true]
		ifFalse: [super handlesMouseDown: evt]
]

{ #category : #'event handling' }
StringMorph >> hasFocus [
	^ hasFocus
]

{ #category : #'*Morphic-Base-Widgets' }
StringMorph >> heightToDisplayInList: aList [

	^ self contents heightToDisplayInList: aList 
]

{ #category : #drawing }
StringMorph >> imageForm: depth forRectangle: rect [
	| canvas |
	canvas := Display defaultCanvasClass extent: rect extent depth: depth.
	
	backgroundColor isTransparent ifTrue: [ 
		canvas form fillColor: self theme backgroundColor.
	].
	canvas translateBy: rect topLeft negated
		during:[:tempCanvas| tempCanvas fullDrawMorph: self].
	^ canvas form offset: rect topLeft
]

{ #category : #initialization }
StringMorph >> initWithContents: aString font: aFont emphasis: emphasisCode [ 
	super initialize.
	
	font := aFont.
	emphasis := emphasisCode.
	hasFocus := false.
	self contents: aString
]

{ #category : #initialization }
StringMorph >> initialize [
	"initialize the state of the receiver"

	super initialize.
	emphasis := 0.
	hasFocus := false.
	backgroundColor := Color transparent
]

{ #category : #accessing }
StringMorph >> interimContents: aString [
	"The receiver is under edit and aString represents the string the user sees as she edits, which typically will not have been accepted and indeed may be abandoned"

	self contents: aString
]

{ #category : #accessing }
StringMorph >> isEditable [
	^ self editorProvider notNil
]

{ #category : #testing }
StringMorph >> isEditable: evt [
	"If the shift key is pressed then yes.
	As normal if StringMorph class>> editableStringMorph  returns false."
	
	^(self isEditable and: [evt shiftPressed])

]

{ #category : #accessing }
StringMorph >> isTranslucentButNotTransparent [
	"Answer true if this any of this morph is translucent but not transparent."
	^ true
]

{ #category : #'private editing' }
StringMorph >> launchEditor: evt [
	" Launch an editor to edit my contents "
	self editorProvider ifNil: [ ^self ].
	self editorProvider openEditorForStringMorph: self
]

{ #category : #editing }
StringMorph >> lostFocusWithoutAccepting [
	"The message is sent when the user, having been in an editing episode on the receiver, changes the keyboard focus -- typically by clicking on some editable text somewhere else -- without having accepted the current edits."

	self acceptContents
]

{ #category : #accessing }
StringMorph >> measureContents [
	"Round up in case fractional."
	
	| f |
	f := self fontToUse.
	^((((f widthOfString: contents) max: self minimumWidth)  @ f height) + (self layoutInset * 2)) ceiling
]

{ #category : #accessing }
StringMorph >> minHeight [
	"Answer the minimum height of the receiver."
	
	^self fontToUse height max: super minHeight
]

{ #category : #accessing }
StringMorph >> minimumWidth [
	"Answer the minimum width that the receiver can have.  A nonzero value here keeps the receiver from degenerating into something that cannot ever be seen or touched again!  Obeyed by fitContents."

	^ 3
]

{ #category : #'event handling' }
StringMorph >> mouseDown: evt [
	"If the shift key is pressed, make this string the keyboard input focus.
	Process as normal if StringMorph class>> editableStringMorph  returns false."

	(self isEditable: evt)
		ifTrue: [self launchEditor: evt]
		ifFalse: [super mouseDown: evt].

]

{ #category : #accessing }
StringMorph >> paneColor [
	"Answer the window's pane color or our owner's color otherwise."

	^self paneColorOrNil ifNil: [self owner ifNil: [Color transparent] ifNotNil: [self owner color]]
]

{ #category : #printing }
StringMorph >> printOn: aStream [

	super printOn: aStream.
	aStream print: contents.

]

{ #category : #private }
StringMorph >> privateSetContents: newContents [ 
	| scanner |
	contents := newContents isText
				ifTrue: [scanner := StringMorphAttributeScanner new initializeFromStringMorph: self.
					(newContents attributesAt: 1 forStyle: self font textStyle)
						do: [:attr | attr emphasizeScanner: scanner].
					emphasis := scanner emphasis.
					font := scanner font emphasis: emphasis.
					color := scanner textColor.
					newContents string]
				ifFalse: [contents = newContents
						ifTrue: [^ self].
					"no substantive change"
					newContents].
]

{ #category : #'*Morphic-Base-MorphTreeWidget' }
StringMorph >> rowMorphForNode: aNode inColumn: aColumn [
	| hasIcon |
	hasIcon := (aColumn container iconBlock value: aNode) notNil.
	(aColumn isFirstColumn and: [hasIcon]) 
		ifTrue: [^ super rowMorphForNode: aNode inColumn: aColumn].
	self layoutInset: aColumn container columnInset @ aColumn container rowInset.
	self fitContents.
	^ self 
]

{ #category : #accessing }
StringMorph >> setWidth: width [
	"Round up in case fractional."
	
	self extent: (width @ (font ifNil: [TextStyle defaultFont]) height) ceiling
]

{ #category : #accessing }
StringMorph >> strikethroughColor [

	^ self stringColor 
]

{ #category : #accessing }
StringMorph >> stringBounds [

	| bnd gap |
	
	bnd := self bounds.
	gap := self layoutInset.
	
	^ (bnd topLeft + gap) corner: (bnd bottomRight + gap)
]

{ #category : #accessing }
StringMorph >> stringColor [

	^ self color
]

{ #category : #initialization }
StringMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]

{ #category : #accessing }
StringMorph >> underlineColor [

	^ self stringColor 
]

{ #category : #accessing }
StringMorph >> userString [
	"Do I have a text string to be searched on?"

	^ contents
]

{ #category : #accessing }
StringMorph >> valueFromContents [
	"Return a new value from the current contents string."
	^ contents
]

{ #category : #editing }
StringMorph >> wantsKeyboardFocusOnShiftClick [
	^ owner topRendererOrSelf wantsKeyboardFocusFor: self

]

{ #category : #'*Morphic-Base-Widgets' }
StringMorph >> widthToDisplayInList: aList [

	^ self contents widthToDisplayInList: aList 
]

{ #category : #'event handling' }
StringMorph >> wouldAcceptKeyboardFocus [
	^ self isLocked not
]
