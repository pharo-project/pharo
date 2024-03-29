Class {
	#name : 'RubDisplayScanner',
	#superclass : 'RubCharacterScanner',
	#instVars : [
		'bitBlt',
		'lineY',
		'runX',
		'foregroundColor',
		'lineHeight',
		'morphicOffset',
		'defaultTextColor',
		'scale'
	],
	#category : 'Rubric-TextScanning',
	#package : 'Rubric',
	#tag : 'TextScanning'
}

{ #category : 'queries' }
RubDisplayScanner class >> defaultFont [
	^ TextStyle defaultFont
]

{ #category : 'stop conditions' }
RubDisplayScanner >> cr [
	"When a carriage return is encountered, simply increment the pointer
	into the paragraph."

	pendingKernX := 0.
	(lastIndex < text size and: [(text at: lastIndex) = Character cr  and: [(text at: lastIndex+1) = Character lf]])
		ifTrue: [lastIndex := lastIndex + 2]
		ifFalse: [lastIndex := lastIndex + 1].
	^false
]

{ #category : 'stop conditions' }
RubDisplayScanner >> crossedX [
	"This condition will sometimes be reached 'legally' during display, when,
	for instance the space that caused the line to wrap actually extends over
	the right boundary. This character is allowed to display, even though it
	is technically outside or straddling the clipping ectangle since it is in
	the normal case not visible and is in any case appropriately clipped by
	the scanner."

	^ true
]

{ #category : 'private' }
RubDisplayScanner >> defaultTextColor [
	defaultTextColor ifNil: [ defaultTextColor := Smalltalk ui theme textColor ].
	^ defaultTextColor
]

{ #category : 'scanning' }
RubDisplayScanner >> displayLine: textLine offset: offset leftInRun: leftInRun [
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."
	| stopCondition nowLeftInRun startIndex string lastPos |
	line := textLine.
	morphicOffset := offset.
	lineY := line top * self scale + offset y.
	lineHeight := line lineHeight * self scale.
	rightMargin := line rightMargin * self scale + offset x.
	lastIndex := line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin := (line leftMarginForAlignment: alignment) * self scale + offset x.
	destX := runX := leftMargin.
	lastIndex := line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	baselineY := lineY + (line baseline * self scale).
	destY := baselineY - (font ascent * self scale).
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := text string.
	[
		startIndex := lastIndex.
		lastPos := destX@destY.
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
						in: string rightX: rightMargin stopConditions: stopConditions
						kern: kern.
		lastIndex >= startIndex ifTrue:[
			font displayString: string on: bitBlt
				from: startIndex
	"XXXX: The following is an interesting bug. All stopConditions exept #endOfRun
		have lastIndex past the last character displayed. #endOfRun sets it *on* the character.
		If we display up until lastIndex then we will also display invisible characters like
		CR and tab. This problem should be fixed in the scanner (i.e., position lastIndex
		consistently) but I don't want to deal with the fallout right now so we keep the
		fix minimally invasive."
				to: (stopCondition == #endOfRun ifTrue:[lastIndex] ifFalse:[lastIndex-1])
				at: lastPos kern: kern baselineY: baselineY scale: self scale].
		(emphasisCode allMask: 4) ifTrue:[
			font displayUnderlineOn: bitBlt from: lastPos x@baselineY to: destX@baselineY scale: self scale.
		].
		(emphasisCode allMask: 16) ifTrue:[
			font displayStrikeoutOn: bitBlt from: lastPos x@baselineY to: destX@baselineY scale: self scale.
		].
		"see setStopConditions for stopping conditions for displaying."
		self perform: stopCondition.
		"or: [lastIndex > runStopIndex]."
	] whileFalse.
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"
]

{ #category : 'stop conditions' }
RubDisplayScanner >> endOfRun [
	"The end of a run in the display case either means that there is actually
	a change in the style (run code) to be associated with the string or the
	end of this line has been reached."
	| runLength |
	lastIndex = line last ifTrue: [^true].
	runX := destX.
	runLength := text runLengthFor: (lastIndex := lastIndex + 1).
	runStopIndex := lastIndex + (runLength - 1) min: line last.
	self setStopConditions.
	^ false
]

{ #category : 'initialization' }
RubDisplayScanner >> initialize [

	super initialize.
	scale := 1
]

{ #category : 'multilingual scanning' }
RubDisplayScanner >> isBreakableAt: index in: sourceString [

	^ false
]

{ #category : 'stop conditions' }
RubDisplayScanner >> paddedSpace [
	"Each space is a stop condition when the alignment is right justified.
	Padding must be added to the base width of the space according to
	which space in the line this space is and according to the amount of
	space that remained at the end of the line when it was composed."

	spaceCount := spaceCount + 1.
	destX := destX + (spaceWidth * self scale) + ((line justifiedPadFor: spaceCount font: font) * scale).
	lastIndex := lastIndex + 1.
	pendingKernX := 0.
	^ false
]

{ #category : 'scanning' }
RubDisplayScanner >> placeEmbeddedObject: anchoredMorph [
	anchoredMorph relativeTextAnchorPosition ifNotNil:[
		anchoredMorph position:
			anchoredMorph relativeTextAnchorPosition +
			(anchoredMorph owner bounds origin x @ 0)
			+ (0@((lineY - morphicOffset y) / self scale)).
		^true
	].
	(super placeEmbeddedObject: anchoredMorph) ifFalse: [^ false].
	anchoredMorph isMorph ifTrue: [
		anchoredMorph position: (((destX - (anchoredMorph width * self scale))@lineY) - morphicOffset) / self scale.
		anchoredMorph setProperty: #hasBeenPositioned toValue: true
	] ifFalse: [
		destY := lineY.
		baselineY := lineY + (anchoredMorph height * self scale).
		runX := destX.
		(self scale = 1 ifTrue: [ anchoredMorph ] ifFalse: [ anchoredMorph scaledToSize: anchoredMorph extent * self scale ])
			displayOn: bitBlt destForm
			at: destX - (anchoredMorph width * self scale) @ destY
			clippingBox: bitBlt clipRect
			rule: Form blend
			fillColor: Color white
	].
	^ true
]

{ #category : 'accessing' }
RubDisplayScanner >> scale [

	^ scale
]

{ #category : 'accessing' }
RubDisplayScanner >> scale: newScale [

	scale := newScale
]

{ #category : 'stop conditions' }
RubDisplayScanner >> setFont [
	foregroundColor := self defaultTextColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	font installOn: bitBlt foregroundColor: foregroundColor backgroundColor: Color transparent scale: self scale.
	text ifNotNil:[
		baselineY := lineY + (line baseline * self scale).
		destY := baselineY - (font ascent * self scale)]
]

{ #category : 'stop conditions' }
RubDisplayScanner >> setPort: aBitBlt [
	"Install the BitBlt to use"
	bitBlt := aBitBlt.
	bitBlt sourceX: 0; width: 0.	"Init BitBlt so that the first call to a primitive will not fail"
	bitBlt sourceForm: nil. "Make sure font installation won't be confused"
]

{ #category : 'stop conditions' }
RubDisplayScanner >> setStopConditions [
	"Set the font and the stop conditions for the current run."

	self setFont.
	self setConditionArray: (alignment = self justified ifTrue: [#paddedSpace]).

"
	alignment = self justified ifTrue: [
		stopConditions == DefaultStopConditions
			ifTrue:[stopConditions := stopConditions copy].
		stopConditions at: Character space asciiValue + 1 put: #paddedSpace]
"
]

{ #category : 'stop conditions' }
RubDisplayScanner >> tab [
	self plainTab.
	lastIndex := lastIndex + 1.
	^ false
]

{ #category : 'private' }
RubDisplayScanner >> text: t textStyle: ts foreground: foreColor [
	text := t.
	textStyle := ts.
	foregroundColor := foreColor
]

{ #category : 'private' }
RubDisplayScanner >> textColor: textColor [
	foregroundColor := textColor
]
