Class {
	#name : #RubCharacterBlockScanner,
	#superclass : #RubCharacterScanner,
	#instVars : [
		'characterPoint',
		'characterIndex',
		'lastCharacter',
		'lastCharacterExtent',
		'lastSpaceOrTabExtent',
		'nextLeftMargin',
		'specialWidth'
	],
	#category : #'Rubric-TextScanning'
}

{ #category : #scanning }
RubCharacterBlockScanner >> characterBlockAtPoint: aPoint index: index in: textLine [
	"This method is the Morphic characterBlock finder.  It combines
	MVC's characterBlockAtPoint:, -ForIndex:, and buildCharcterBlock:in:"

	| runLength lineStop stopCondition |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	self setStopConditions.	"also sets font"
	characterIndex := index.	" == nil means scanning for point"
	characterPoint := aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint := line bottomRight ].
	(text isEmpty
		or: [ 
			(characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ] ] ])
		ifTrue: [ 
			^ (RubCharacterBlock new stringIndex: line first topLeft: line leftMargin @ line top extent: 0 @ textStyle lineGrid)
				textLine: line ].
	destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	characterIndex
		ifNotNil: [ lineStop := characterIndex	"scanning for index" ]
		ifNil: [ lineStop := line last	"scanning for point" ].
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterExtent := 0 @ line lineHeight.
	spaceCount := 0.
	[ false ]
		whileFalse: [ 
			stopCondition := self
				scanCharactersFrom: lastIndex
				to: runStopIndex
				in: text string
				rightX: characterPoint x
				stopConditions: stopConditions
				kern: kern.	"see setStopConditions for stopping conditions for character block 	operations."
			self lastCharacterExtentSetX: (specialWidth ifNil: [ font widthOf: (text at: lastIndex) ] ifNotNil: [ specialWidth ]).
			(self perform: stopCondition)
				ifTrue: [ 
					characterIndex
						ifNil: [ 
							"Result for characterBlockAtPoint: "
							(stopCondition ~~ #cr
								and: [ lastIndex == line last and: [ aPoint x > (characterPoint x + (lastCharacterExtent x / 2)) ] ])
								ifTrue: [ 
									"Correct for right half of last character in line"
									^ (RubCharacterBlock new
										stringIndex: lastIndex + 1
										topLeft: characterPoint + (lastCharacterExtent x @ 0) + (font descentKern @ 0)
										extent: 0 @ lastCharacterExtent y) textLine: line ].
							^ (RubCharacterBlock new
								stringIndex: lastIndex
								topLeft: characterPoint + (font descentKern @ 0)
								extent: lastCharacterExtent - (font baseKern @ 0)) textLine: line ]
						ifNotNil: [ 
							"Result for characterBlockForIndex: "
							^ (RubCharacterBlock new
								stringIndex: characterIndex
								topLeft: characterPoint + ((font descentKern - kern) @ 0)
								extent: lastCharacterExtent) textLine: line ] ] ]
]

{ #category : #private }
RubCharacterBlockScanner >> characterPointSetX: xVal [
	characterPoint := xVal @ characterPoint y
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> cr [ 
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
				baselineY := line lineHeight.
				lastCharacter := nil.
				characterPoint := (nextLeftMargin ifNil: [leftMargin]) @ destY.
				lastIndex := (lastIndex < text size and: [(text at: lastIndex) = Character cr and: [(text at: lastIndex+1) = Character lf]])
					ifTrue: [ lastIndex + 2 ]
					ifFalse: [ lastIndex + 1 ].
				self lastCharacterExtentSetX: 0.
				^ true].
		lastCharacter := Character cr.
		characterPoint := destX @ destY.
		self lastCharacterExtentSetX: rightMargin - destX.
		^true
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> crossedX [
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| leadingTab currentX |
	characterIndex == nil ifFalse: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex := characterIndex.
			characterPoint := (nextLeftMargin ifNil: [leftMargin]) @ (destY + line lineHeight).
			^true]].
	characterPoint x <= (destX + (lastCharacterExtent x // 2))
		ifTrue:	[lastCharacter := (text at: lastIndex).
				characterPoint := destX @ destY.
				^true].
	lastIndex >= line last 
		ifTrue:	[lastCharacter := (text at: line last).
				characterPoint := destX @ destY.
				^true].

	"Pointing past middle of a character, return the next character."
	lastIndex := lastIndex + 1.
	lastCharacter := text at: lastIndex.
	currentX := destX + lastCharacterExtent x + kern.
	self lastCharacterExtentSetX: (font widthOf: lastCharacter).
	characterPoint := currentX @ destY.
	lastCharacter = Space ifFalse: [^ true].

	"Yukky if next character is space or tab."
	alignment = Justified ifTrue:
		[self lastCharacterExtentSetX:
			(lastCharacterExtent x + 	(line justifiedPadFor: (spaceCount + 1) font: font)).
		^ true].

	true ifTrue: [^ true].
	"NOTE:  I find no value to the following code, and so have defeated it - DI"

	"See tabForDisplay for illumination on the following awfulness."
	leadingTab := true.
	line first to: lastIndex - 1 do:
		[:index | (text at: index) ~= Tab ifTrue: [leadingTab := false]].
	(alignment ~= Justified or: [leadingTab])
		ifTrue:	[self lastCharacterExtentSetX: (self nextTabXFrom: currentX) -
						currentX]
		ifFalse:	[self lastCharacterExtentSetX:  (((currentX + (self tabWidth -
						(line justifiedTabDeltaFor: spaceCount))) -
							currentX) max: 0)].
	^ true
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> endOfRun [
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
				runLength := (text runLengthFor: (lastIndex := lastIndex + 1)).
				characterIndex ~~ nil
					ifTrue:	[lineStop := characterIndex	"scanning for index"]
					ifFalse:	[lineStop := line last			"scanning for point"].
				(runStopIndex := lastIndex + (runLength - 1)) > lineStop
					ifTrue: 	[runStopIndex := lineStop].
				self setStopConditions.
				^false].

	lastCharacter := text at: lastIndex.
	characterPoint := destX @ destY.
	((lastCharacter = Space and: [alignment = Justified])
		or: [lastCharacter = Tab and: [lastSpaceOrTabExtent notNil]])
		ifTrue: [lastCharacterExtent := lastSpaceOrTabExtent].
	characterIndex ~~ nil
		ifTrue:	["If scanning for an index and we've stopped on that index,
				then we back destX off by the width of the character stopped on
				(it will be pointing at the right side of the character) and return"
				runStopIndex = characterIndex
					ifTrue:	[self characterPointSetX: destX - lastCharacterExtent x.
							^true].
				"Otherwise the requested index was greater than the length of the
				string.  Return string size + 1 as index, indicate further that off the
				string by setting character to nil and the extent to 0."
				lastIndex :=  lastIndex + 1.
				lastCharacter := nil.
				self lastCharacterExtentSetX: 0.
				^true].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue:	["off end of string"
				lastIndex :=  lastIndex + 1.
				lastCharacter := nil.
				self lastCharacterExtentSetX: 0.
				^true].
	"just off end of line without crossing x"
	lastIndex := lastIndex + 1.
	^true
]

{ #category : #scanning }
RubCharacterBlockScanner >> indentationLevel: anInteger [
	super indentationLevel: anInteger.
	nextLeftMargin := leftMargin.
	indentationLevel timesRepeat: [
		nextLeftMargin := self nextTabXFrom: nextLeftMargin]
]

{ #category : #private }
RubCharacterBlockScanner >> lastCharacterExtentSetX: xVal [
	lastCharacterExtent := xVal @ lastCharacterExtent y
]

{ #category : #private }
RubCharacterBlockScanner >> lastSpaceOrTabExtentSetX: xVal [
	lastSpaceOrTabExtent := xVal @ lastSpaceOrTabExtent y
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> paddedSpace [
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	pad := 0.
	spaceCount := spaceCount + 1.
	pad := line justifiedPadFor: spaceCount font: font.
	lastSpaceOrTabExtent := lastCharacterExtent copy.
	self lastSpaceOrTabExtentSetX:  spaceWidth + pad.
	(destX + lastSpaceOrTabExtent x)  >= characterPoint x
		ifTrue: [lastCharacterExtent := lastSpaceOrTabExtent copy.
				^self crossedX].
	lastIndex := lastIndex + 1.
	destX := destX + lastSpaceOrTabExtent x.
	^ false

]

{ #category : #scanning }
RubCharacterBlockScanner >> placeEmbeddedObject: anchoredMorph [
	"Workaround: The following should really use #textAnchorType"
	anchoredMorph relativeTextAnchorPosition ifNotNil:[^true].
	(super placeEmbeddedObject: anchoredMorph) ifFalse: [^ false].
	specialWidth := anchoredMorph width.
	^ true
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> setFont [
	specialWidth := nil.
	super setFont
]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> setStopConditions [
	"Set the font and the stop conditions for the current run."
	
	self setFont.
	self setConditionArray: (alignment = Justified ifTrue: [#paddedSpace]).

]

{ #category : #'stop conditions' }
RubCharacterBlockScanner >> tab [
	| currentX |
	currentX := (alignment = Justified and: [self leadingTab not])
		ifTrue:		"imbedded tabs in justified text are weird"
			[destX + (self tabWidth - (line justifiedTabDeltaFor: spaceCount)) max: destX]
		ifFalse:
			[self nextTabXFrom: destX].
	lastSpaceOrTabExtent := lastCharacterExtent copy.
	self lastSpaceOrTabExtentSetX: (currentX - destX max: 0).
	currentX >= characterPoint x
		ifTrue: 
			[lastCharacterExtent := lastSpaceOrTabExtent copy.
			^ self crossedX].
	destX := currentX.
	lastIndex := lastIndex + 1.
	^false
]
