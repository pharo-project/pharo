"
A DisplayScanner is an abstract class for displaying characters.
It is splitting text into elementary chunks of displayable String/Font pairs (see scanning protocol).
Subclasses responsibility is to handle the effective rendering of these chunks on various backends.

Instance Variables
	backgroundColor:		<Color>
	defaultTextColor:		<Color>
	foregroundColor:		<Color>
	ignoreColorChanges:		<Boolean>
	lastDisplayableIndex:		<Integer>
	lineY:		<Number>
	morphicOffset:		<Point>
	stopConditionsMustBeReset:		<Boolean>

backgroundColor
	- the background color for displaying next chunk of text.
	Note that this can be set to Color transparent, in which case no background is displayed.

defaultTextColor
	- the default foreground color for displaying text in absence of other text attributes specification 

foregroundColor
	- the foreground color for displaying next chunk of text

ignoreColorChanges
	- indicates that any change of color specified in text attributes shall be ignored.
	This is used for displaying text in a shadow mode, when dragging text for example.

lastDisplayableIndex
	- the index of last character to be displayed.
	A different index than lastIndex is required in order to avoid display of control characters.
	This variable must be updated by the stop condition at each inner scan loop.

lineY
	- the distance between destination form top and current line top

morphicOffset
	- an offset for positionning the embedded morphs.
	THE EXACT SPECIFICATION YET REMAINS TO BE WRITTEN

stopConditionsMustBeReset
	- indicates that it's necessary to call setStopConditions in next scan loop.

Notes:
In order to correctly set the lastDisplayableIndex, the display scanner performs the stopCondition BEFORE displaying the string being scanned.
This explains why the stopCondition must not reset the font immediately, but differ this reset AFTER the display, thanks to stopConditionsMustBeReset.

"
Class {
	#name : #DisplayScanner,
	#superclass : #CharacterScanner,
	#instVars : [
		'lineY',
		'foregroundColor',
		'backgroundColor',
		'defaultTextColor',
		'morphicOffset',
		'ignoreColorChanges',
		'lastDisplayableIndex',
		'stopConditionsMustBeReset'
	],
	#category : #'Text-Scanning-Base'
}

{ #category : #queries }
DisplayScanner class >> defaultFont [
	^ TextStyle defaultFont
]

{ #category : #'instance creation' }
DisplayScanner class >> new [
	"Use default concrete class"
	^(self == DisplayScanner
		ifTrue: [BitBltDisplayScanner]
		ifFalse: [self]) basicNew initialize
]

{ #category : #'stop conditions' }
DisplayScanner >> cr [
	"When a carriage return is encountered, simply increment the pointer 
	into the paragraph."

	pendingKernX := 0.
	lastDisplayableIndex := lastIndex - 1.
	(lastIndex < text size and: [(text at: lastIndex) = CR and: [(text at: lastIndex+1) = Character lf]])
		ifTrue: [lastIndex := lastIndex + 2]
		ifFalse: [lastIndex := lastIndex + 1].
	^false
]

{ #category : #'stop conditions' }
DisplayScanner >> crossedX [
	"This condition will sometimes be reached 'legally' during display, when, 
	for instance the space that caused the line to wrap actually extends over 
	the right boundary. This character is allowed to display, even though it 
	is technically outside or straddling the clipping rectangle since it is in 
	the normal case not visible and is in any case appropriately clipped by 
	the scanner."

	self advanceIfFirstCharOfLine.
	lastDisplayableIndex := lastIndex - 1.
	^ true 
]

{ #category : #private }
DisplayScanner >> defaultTextColor [
	^ defaultTextColor ifNil:[ defaultTextColor := Smalltalk ui theme textColor ]
]

{ #category : #private }
DisplayScanner >> defaultTextColor: color [
	defaultTextColor := color.
]

{ #category : #displaying }
DisplayScanner >> displayEmbeddedForm: aForm [
	self subclassResponsibility
]

{ #category : #scanning }
DisplayScanner >> displayLine: textLine offset: offset leftInRun: leftInRun [
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."
	| stopCondition nowLeftInRun startIndex string lastPos lineHeight stop |
	line := textLine.
	morphicOffset := offset.
	lineY := line top + offset y.
	lineHeight := line lineHeight.
	rightMargin := line rightMargin + offset x.
	lastIndex := line first.
	leftInRun <= 0 ifTrue: [self setStopConditions].
	leftMargin := (line leftMarginForAlignment: alignment) + offset x.
	destX := leftMargin.
	self fillTextBackground.
	lastDisplayableIndex := lastIndex := line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	destY := lineY + line baseline - font ascent.
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := text string.
	[
		"reset the stopping conditions of this displaying loop, and also the font."
		stopConditionsMustBeReset
			ifTrue:[self setStopConditions].
		
		"remember where this portion of the line starts"
		startIndex := lastIndex.
		lastPos := destX@destY.
		
		"find the end of this portion of the line"
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
						in: string rightX: rightMargin.
		"handle the stop condition - this will also set lastDisplayableIndex"
		stop := self perform: stopCondition.
		
		"display that portion of the line"
		lastDisplayableIndex >= startIndex ifTrue:[
			self displayString: string
				from: startIndex 
				to: lastDisplayableIndex 
				at: lastPos].
		
		"if the stop condition were true, stop the loop"
		stop
	] whileFalse.
	^ runStopIndex - lastIndex   "Number of characters remaining in the current run"
]

{ #category : #displaying }
DisplayScanner >> displayString: string from: startIndex  to: stopIndex at: aPoint [
	self subclassResponsibility
]

{ #category : #'stop conditions' }
DisplayScanner >> endOfRun [
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastDisplayableIndex := lastIndex.
	lastIndex = line last ifTrue: [^true].
	runLength := text runLengthFor: (lastIndex := lastIndex + 1).
	runStopIndex := lastIndex + (runLength - 1) min: line last.
	"differ reset of stopConditions and font AFTER the dispaly of last scanned string"
	stopConditionsMustBeReset := true.
	^ false
]

{ #category : #displaying }
DisplayScanner >> fillTextBackground [
	self subclassResponsibility
]

{ #category : #initialization }
DisplayScanner >> initialize [
	super initialize.
	ignoreColorChanges := false.
]

{ #category : #'stop conditions' }
DisplayScanner >> paddedSpace [
	"Each space is a stop condition when the alignment is right justified. 
	Padding must be added to the base width of the space according to 
	which space in the line this space is and according to the amount of 
	space that remained at the end of the line when it was composed."

	lastDisplayableIndex := lastIndex - 1.
	spaceCount := spaceCount + 1.
	destX := destX + spaceWidth + kern + (line justifiedPadFor: spaceCount font: font).
	lastIndex := lastIndex + 1.
	pendingKernX := 0.
	^ false
]

{ #category : #private }
DisplayScanner >> placeEmbeddedObject: anchoredMorphOrForm [

	anchoredMorphOrForm relativeTextAnchorPosition ifNotNil:[:relativeTextAnchorPosition |
		anchoredMorphOrForm position: 
			relativeTextAnchorPosition +
			(anchoredMorphOrForm owner textBounds origin x @ (lineY - morphicOffset y)).
		^true
	].
	anchoredMorphOrForm isMorph ifTrue: [
		anchoredMorphOrForm position: (destX@(lineY + line baseline - anchoredMorphOrForm height)) - morphicOffset
	] ifFalse: [
		self displayEmbeddedForm: anchoredMorphOrForm
	].
	destX := destX + anchoredMorphOrForm width + kern.
	^ true
]

{ #category : #private }
DisplayScanner >> setFont [ 
	foregroundColor := self defaultTextColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil:[destY := lineY + line baseline - font ascent]
]

{ #category : #private }
DisplayScanner >> setStopConditions [
	super setStopConditions.
	stopConditionsMustBeReset := false
]

{ #category : #'stop conditions' }
DisplayScanner >> tab [
	lastDisplayableIndex := lastIndex - 1.
	destX := self plainTab.
	lastIndex := lastIndex + 1.
	^ false
]

{ #category : #private }
DisplayScanner >> text: t textStyle: ts foreground: foreColor background: backColor fillBlt: blt ignoreColorChanges: shadowMode [
	text := t.
	textStyle := ts. 
	foregroundColor := defaultTextColor := foreColor.
	backgroundColor := backColor.
	ignoreColorChanges := shadowMode
]

{ #category : #'text attributes' }
DisplayScanner >> textColor: textColor [
	ignoreColorChanges ifTrue: [^ self].
	foregroundColor := textColor
]
