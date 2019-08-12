"
A CharacterScanner does scan text to compute the CharacterBlock for a character specified by its index in the text or its proximity to the cursor location. The CharacterBlock stores information both about character layout and character index in the text.

This class is essential for selecting text with the mouse or with arrow keys.

Instance Variables
	characterIndex:		<Integer | nil>
	characterPoint:		<Point>
	lastCharacterWidth:		<Number | nil>
	nextLeftMargin:		<Number>
	specialWidth:		<Number | nil>

characterIndex
	- the index of character for which the layout information is searched, or nil when the layout is searched by cursor location

characterPoint
	- the cursor location for which nearest character index and layout are searched.

lastCharacterWidth
	- a number indicating the width of last character being processed.
	Note that this variable is left to nil during the inner scan loop, and only set on stopConditions.

nextLeftMargin
	- a number specifying the distance between left of composition zone and left of first character for the next line.

specialWidth
	- a number holding the width of an embedded object if any, or nil if none.

"
Class {
	#name : #CharacterBlockScanner,
	#superclass : #CharacterScanner,
	#instVars : [
		'characterPoint',
		'characterIndex',
		'nextLeftMargin',
		'specialWidth',
		'lastCharacterWidth'
	],
	#category : #'Text-Scanning-Base'
}

{ #category : #scanning }
CharacterBlockScanner >> characterBlockAtPoint: aPoint index: index in: textLine [
	"This method is the Morphic characterBlock finder.  It combines
	MVC's characterBlockAtPoint:, -ForIndex:, and buildCharacterBlockIn:"
	| runLength lineStop stopCondition |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	self setStopConditions.		"also sets font"
	characterIndex := index.  " == nil means scanning for point"
	characterPoint := aPoint.
	(characterPoint isNil or: [characterPoint y > line bottom])
		ifTrue: [characterPoint := line bottomRight].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	(text isEmpty or: [(characterPoint y < destY or: [characterPoint x < destX])
				or: [characterIndex notNil and: [characterIndex < line first]]])
		ifTrue:	[^ (CharacterBlock new stringIndex: line first text: text
					topLeft: destX@destY extent: 0 @ textStyle lineGrid)
					textLine: line].
	runLength := text runLengthFor: line first.
	lineStop := characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.

	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x.
		"see setStopConditions for stopping conditions for character block operations."
		self perform: stopCondition
	] whileFalse.
	characterIndex
		ifNil: ["Result for characterBlockAtPoint: "
				^ (CharacterBlock new
					stringIndex: lastIndex
					text: text topLeft: characterPoint + (font descentKern @ 0)
					extent: lastCharacterWidth @ line lineHeight - (font baseKern @ 0))
							textLine: line]
		ifNotNil: ["Result for characterBlockForIndex: "
				^ (CharacterBlock new
					stringIndex: characterIndex
					text: text topLeft: characterPoint + ((font descentKern) - kern @ 0)
					extent: lastCharacterWidth @ line lineHeight)
							textLine: line]
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> cr [ 
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a carriage return stop condition that has just been 
	encountered. The ParagraphEditor convention is to denote selections by 
	CharacterBlocks, sometimes including the carriage return (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue:	["When off end of string, give data for next character"
				destY := destY +  line lineHeight.
				characterPoint := (nextLeftMargin ifNil: [leftMargin]) @ destY.
				lastIndex := (lastIndex < text size and: [(text at: lastIndex) = CR and: [(text at: lastIndex+1) = Character lf]])
					ifTrue: [ lastIndex + 2]
					ifFalse: [ lastIndex + 1].
				lastCharacterWidth := 0.
				^ true].
		characterPoint := destX @ destY.
		lastCharacterWidth := rightMargin - destX.
		^true
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> crossedX [
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	self retrieveLastCharacterWidth.
	
	characterPoint x <= (destX + (lastCharacterWidth // 2))
		ifTrue:	[characterPoint := destX @ destY.
				^true].
	lastIndex >= line last 
		ifTrue:	[characterPoint := destX @ destY.
				^true].

	"Pointing past middle of a character, return the next character."
	lastIndex := lastIndex + 1.
	characterPoint := destX + lastCharacterWidth + kern @ destY.
	^ true
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> endOfRun [
	"Before arriving at the cursor location, the selection has encountered an 
	end of run. Answer false if the selection continues, true otherwise. Set 
	up indexes for building the appropriate CharacterBlock."

	| runLength lineStop |
	
	(((characterIndex ~~ nil and:
		[runStopIndex < characterIndex and: [runStopIndex < text size]])
			or:	[characterIndex == nil and: [lastIndex < line last]]) or: [
				((lastIndex < line last)
				and: [((text at: lastIndex) leadingChar ~= (text at: lastIndex+1) leadingChar)
					and: [lastIndex ~= characterIndex]])])
		ifTrue:	["We're really at the end of a real run."
				runLength := text runLengthFor: (lastIndex := lastIndex + 1).
				lineStop := characterIndex	"scanning for index"
						ifNil: [line last].		"scanning for point".
				(runStopIndex := lastIndex + (runLength - 1)) > lineStop
					ifTrue: [runStopIndex := lineStop].
				self setStopConditions.
				^false].

	self retrieveLastCharacterWidth.
		
	(characterIndex == nil and: [lastIndex = line last])
		ifTrue: [characterPoint x > (destX + (lastCharacterWidth // 2))
			ifTrue:
				[ "Correct for clicking right half of last character in line
				means selecting AFTER the char"
				lastIndex := lastIndex + 1.
				lastCharacterWidth := 0.
				characterPoint := destX + lastCharacterWidth @ destY.
				^true]].

	characterPoint := destX @ destY.
	characterIndex ~~ nil
		ifTrue:	["If scanning for an index and we've stopped on that index,
				then we back destX off by the width of the character stopped on
				(it will be pointing at the right side of the character) and return"
				runStopIndex = characterIndex
					ifTrue:	[characterPoint := destX - lastCharacterWidth @ destY.
							^true].
				"Otherwise the requested index was greater than the length of the
				string.  Return string size + 1 as index, indicate further that off the
				string by setting character to nil and the extent to 0."
				lastIndex :=  lastIndex + 1.
				lastCharacterWidth := 0.
				^true].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue:	["off end of string"
				lastIndex :=  lastIndex + 1.
				lastCharacterWidth := 0.
				^true].
	"just off end of line without crossing x"
	lastIndex := lastIndex + 1.
	^true
]

{ #category : #'text attributes' }
CharacterBlockScanner >> indentationLevel: anInteger [
	super indentationLevel: anInteger.
	nextLeftMargin := leftMargin.
	indentationLevel timesRepeat: [
		nextLeftMargin := textStyle nextTabXFrom: nextLeftMargin
					leftMargin: leftMargin
					rightMargin: rightMargin]
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> paddedSpace [
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	spaceCount := spaceCount + 1.
	pad := line justifiedPadFor: spaceCount font: font.
	lastCharacterWidth := spaceWidth + pad.
	(destX + lastCharacterWidth)  >= characterPoint x
		ifTrue:
			[^self crossedX].
	lastIndex := lastIndex + 1.
	destX := destX + lastCharacterWidth + kern.
	pendingKernX := 0.
	^ false

]

{ #category : #private }
CharacterBlockScanner >> placeEmbeddedObject: anchoredMorph [
	"Workaround: The following should really use #textAnchorType"
	| w |
	anchoredMorph relativeTextAnchorPosition ifNotNil:[^true].
	w := anchoredMorph width.
	specialWidth := w.
	(destX + w > characterPoint x) ifTrue: [^false].
	destX := destX + w + kern.
	^ true
]

{ #category : #private }
CharacterBlockScanner >> retrieveLastCharacterWidth [
	| lastCharacter |
	lastIndex > text size ifTrue: [^lastCharacterWidth := 0].
	specialWidth ifNotNil: [^lastCharacterWidth := specialWidth].
	lastCharacter := text at: lastIndex.
	(lastCharacter charCode >= 256 or: [(stopConditions at: lastCharacter charCode + 1) isNil])
		ifTrue: [lastCharacterWidth := font widthOf: (text at: lastIndex)].
	"if last character was a stop condition, then the width is already set"
	^lastCharacterWidth
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> setFont [
	specialWidth := nil.
	super setFont
]

{ #category : #'stop conditions' }
CharacterBlockScanner >> tab [
	| nextDestX |
	nextDestX := self plainTab.
	lastCharacterWidth := nextDestX - destX max: 0.
	nextDestX >= characterPoint x
		ifTrue: 
			[^ self crossedX].
	destX := nextDestX.
	lastIndex := lastIndex + 1.
	^false
]
