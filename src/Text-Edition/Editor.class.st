"
New text editors.
TextEditor provides most of the functionality that used to be in TextMorphEditor. This class is no longer a Controller!
SmalltalkEditor has Smalltalk code specific features.
SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.

"
Class {
	#name : #Editor,
	#superclass : #Object,
	#instVars : [
		'morph',
		'selectionShowing'
	],
	#classVars : [
		'BlinkingCursor',
		'CmdKeysInText',
		'DumbbellCursor',
		'SkipOverMultipleSpaces'
	],
	#category : 'Text-Edition'
}

{ #category : #settings }
Editor class >> blinkingCursor [	
	^ BlinkingCursor ifNil: [ BlinkingCursor := true ]
]

{ #category : #settings }
Editor class >> blinkingCursor: aBoolean [	
	BlinkingCursor := aBoolean
]

{ #category : #settings }
Editor class >> cmdKeysInText [
	^ CmdKeysInText ifNil: [CmdKeysInText := true]
]

{ #category : #settings }
Editor class >> cmdKeysInText: aBoolean [
	CmdKeysInText := aBoolean
]

{ #category : #settings }
Editor class >> dumbbellCursor [
	^ DumbbellCursor ifNil: [ DumbbellCursor := false ]
]

{ #category : #settings }
Editor class >> dumbbellCursor: aBoolean [
	DumbbellCursor := aBoolean
]

{ #category : #settings }
Editor class >> editingSettingsOn: aBuilder [ 
	<systemsettings> 
	(aBuilder group: #codeEditing) 
		label: 'Text Editing';
		noOrdering;
		description: 'All settings concerned with text editing' ;
		with: [	
			(aBuilder setting: #selectionColor)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Selection color'.
			(aBuilder setting: #unfocusedSelectionColor)
				target: UITheme;
				targetSelector: #currentSettings;
				description: 'The color of the selection for unfocused windows' ;
				label: 'Unfocused selection color'.
			(aBuilder setting: #selectionTextColor)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Selection text color';
				description: 'The color of the selection text'.
			(aBuilder setting: #useSelectionBar)
				target: TextEditor;
				label: 'Show the colored bar on the current line';
				with: [
					(aBuilder setting: #selectionBarColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Selection bar color'].
			(aBuilder setting: #useSecondarySelection)
				target: TextEditor;
				label: 'Use the secondary selection';
				with: [
					(aBuilder setting: #secondarySelectionColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Secondary selection color'.
					(aBuilder setting: #secondarySelectionTextColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Secondary selection text color'].
			(aBuilder setting: #useFindReplaceSelection)
				target: TextEditor;
				label: 'Use the find and replace selection';
				with: [
					(aBuilder setting: #findReplaceSelectionColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Find replace selection color'.
					(aBuilder setting: #findReplaceSelectionTextColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Find replace selection text color'].
			(aBuilder setting: #caseSensitiveFinds) 
				label: 'Case sensitive search';
				target: TextEditor;
		 		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion'.
			(aBuilder setting: #blinkingCursor) 
				label: 'Blinking text cursor';
				target: Editor;
		 		description: 'When true, the text cursor will blink.'.
			(aBuilder setting: #dumbbellCursor) 
				label: 'Dumbbell-shaped text cursor';
				target: Editor;
		 		description: 'When true, the text cursor assumes the shape of a dumbbell, otherwise a vertical bar.'.
			(aBuilder setting: #skipOverMultipleSpaces) 
				label: 'Skip over white space';
				target: Editor;
		 		description: 'When true, the text cursor treats multiple white-space characters as a single space for navigating (moving left/right).'.
			(aBuilder setting: #walkAlongDisplayedLine) 
				label: 'Walk along displayed line';
				target: TextEditor;
		 		description: 'If set to true cursor movement will move along displayed lines rather than logical lines'.
		]

]

{ #category : #settings }
Editor class >> skipOverMultipleSpaces [
	^ SkipOverMultipleSpaces  ifNil: [ SkipOverMultipleSpaces := false ]
]

{ #category : #settings }
Editor class >> skipOverMultipleSpaces: aBoolean [
	SkipOverMultipleSpaces := aBoolean
]

{ #category : #'private accessing' }
Editor class >> specialShiftCmdKeys [

"Private - return array of key codes that represent single keys acting
as if shift-command were also being pressed"

^#(
	1	"home"
	3	"enter"
	4	"end"
	8	"backspace"
	11	"page up"
	12	"page down"
	27	"escape"
	28	"left arrow"
	29	"right arrow"
	30	"up arrow"
	31	"down arrow"
	127	"delete"
	)
]

{ #category : #'typing/selecting keys' }
Editor >> backWord: aKeyboardEvent [
	"If the selection is not a caret, delete it and leave it in the backspace buffer.
	 Else, delete the word before the caret."

	| startIndex |
	self hasCaret ifTrue: [ "a caret, delete at least one character"
		startIndex := 1 max: self markIndex - 1.
		[startIndex > 1 and:
			[(self string at: startIndex - 1) tokenish]]
				whileTrue: [
					startIndex := startIndex - 1]]
	ifFalse: [ "a non-caret, just delete it"
		startIndex := self markIndex].
	self backTo: startIndex.
	^false
]

{ #category : #'typing/selecting keys' }
Editor >> backspace: aKeyboardEvent [
	"Backspace over the last character."

	| startIndex |
	aKeyboardEvent shiftPressed ifTrue: [^ self backWord: aKeyboardEvent keyCharacter].
	self hasSelection
		ifTrue: [self replaceSelectionWith: self nullText]
		ifFalse: [startIndex := self markIndex + (self hasCaret ifTrue: [0] ifFalse: [1]).
			startIndex := 1 max: startIndex - 1.
			self backTo: startIndex].
	^false
]

{ #category : #'menu messages' }
Editor >> clipboardText [

	^ Clipboard clipboardText
]

{ #category : #'menu messages' }
Editor >> clipboardTextPut: text [

	^ Clipboard clipboardText: text
]

{ #category : #settings }
Editor >> cmdKeysInText [
	^ self class cmdKeysInText
]

{ #category : #'editing keys' }
Editor >> copySelection: aKeyboardEvent [
	"Copy the current text selection."

	self copySelection.
	^true
]

{ #category : #'typing/selecting keys' }
Editor >> crlf: aKeyboardEvent [
	"Append a line feed character to the stream of characters."

	self addString: String crlf.
	^false
]

{ #category : #accessing }
Editor >> currentAttributes [
	"Redefined by subclasses that handle TextAttributes"
	^nil
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorDown: aKeyboardEvent [ 

	"Private - Move cursor from position in current line to same position in
	next line. If next line too short, put at end. If shift key down,
	select."

	self closeTypeIn.
	self 
		moveCursor:[:position | self
				sameColumn: position
				newLine:[:line | line + 1]
				forward: true]
		forward: true
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorLeft: aKeyboardEvent [ 
	"Private - Move cursor left one character if nothing selected, otherwise 
	move cursor to beginning of selection. If the shift key is down, start 
	selecting or extending current selection. Don't allow cursor past 
	beginning of text"

	self closeTypeIn.
	self      
		moveCursor:[:position | 
			self class skipOverMultipleSpaces 
				ifTrue: [ self previousNonBlank: position ]
				ifFalse: [ position - 1 max: 1 ]
			 ]
		forward: false
		specialBlock:[:position | self previousWord: position]
		event: aKeyboardEvent.
	^ true
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorPageDown: aKeyboardEvent [ 

	self closeTypeIn.
	self 
		moveCursor: [:position |
			self
				sameColumn: position
				newLine: [:lineNo | lineNo + self pageHeight]
				forward: true]
		forward: true
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorPageUp: aKeyboardEvent [ 

	self closeTypeIn.
	self 
		moveCursor: [:position |
			self
				sameColumn: position
				newLine: [:lineNo | lineNo - self pageHeight]
				forward: false]
		forward: false
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorRight: aKeyboardEvent [ 
	"Private - Move cursor right one character if nothing selected, 
	otherwise move cursor to end of selection. If the shift key is down, 
	start selecting characters or extending already selected characters. 
	Don't allow cursor past end of text"

	self closeTypeIn.
	self
		moveCursor: [:position |
			self class skipOverMultipleSpaces 
				ifTrue: [ self nextNonBlank: position ] 
				ifFalse: [ position + 1 ]
			]
		forward: true
		specialBlock:[:position | self nextWord: position]
		event: aKeyboardEvent.
	^ true
]

{ #category : #'typing/selecting keys' }
Editor >> cursorTopHome: aKeyboardEvent [
	"Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key."

	self selectAt: 1.
	^ true
]

{ #category : #'nonediting/nontyping keys' }
Editor >> cursorUp: aKeyboardEvent [ 
"Private - Move cursor from position in current line to same position in
prior line. If prior line too short, put at end"

	self closeTypeIn.
	self
		moveCursor: [:position | self
				sameColumn: position
				newLine:[:line | line - 1]
				forward: false]
		forward: false
		specialBlock:[:dummy | dummy]
		event: aKeyboardEvent.
	^true
]

{ #category : #'editing keys' }
Editor >> cut: aKeyboardEvent [ 
	"Cut out the current text selection."

	self cut.
	^true
]

{ #category : #'current selection' }
Editor >> deselect [
	"If the text selection is visible on the screen, reverse its highlight."
	" ***** screw this logic ***** selectionShowing ifTrue: [self reverseSelection] "
]

{ #category : #'nonediting/nontyping keys' }
Editor >> escape [ 
	self morph escapePressed .
	
]

{ #category : #'nonediting/nontyping keys' }
Editor >> escape: aKeyboardEvent [
	self morph escapePressed.
	^ false  
]

{ #category : #testing }
Editor >> hasError [
	^ false 
]

{ #category : #accessing-selection }
Editor >> hasSelection [
	^self hasCaret not
]

{ #category : #testing }
Editor >> isSimpleEditor [ 
	^ false 
]

{ #category : #testing }
Editor >> isSmalltalkEditor [
	^ false 
]

{ #category : #testing }
Editor >> isTextEditor [
	^ false 
]

{ #category : #'typing/selecting keys' }
Editor >> lf: aKeyboardEvent [ 
	"Append a line feed character to the stream of characters."

	self addString: Character lf asString.
	^false
]

{ #category : #'menu messages' }
Editor >> lineSelectAndEmptyCheck: returnBlock [
	"If the current selection is an insertion point, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is an insertion point, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]
]

{ #category : #private }
Editor >> lines [
	"Compute lines based on logical line breaks, not optical (which may change due to line wrapping of the editor).
	Subclasses using kinds of Paragraphs can instead use the service provided by it.
	"
	| lines string index lineIndex stringSize |
	string := self string.
	"Empty strings have no lines at all. Think of something."
	string isEmpty ifTrue:[^{#(1 0 0)}].
	stringSize := string size.
	lines := OrderedCollection new: (string size // 15).
	index := 0.
	lineIndex := 0.
	string linesDo:[:line |
		lines addLast: (Array
			with: (index := index + 1)
			with: (lineIndex := lineIndex + 1)
			with: (index := index + line size min: stringSize))].
	"Special workaround for last line empty."
	string last == Character cr
	"lines last last < stringSize" ifTrue:[lines addLast:{stringSize +1. lineIndex+1. stringSize}].
	^lines
]

{ #category : #accessing }
Editor >> markBlock: aCharacterBlock [ 
	self editingState markBlock:  aCharacterBlock
]

{ #category : #accessing }
Editor >> morph [
	^ morph
]

{ #category : #accessing }
Editor >> morph: aMorph [
	"Install a link back to the morph being edited (esp for text links)"
	morph := aMorph 
]

{ #category : #private }
Editor >> moveCursor: directionBlock forward: forward specialBlock: specialBlock event: aKeyboardEvent [
	"Private - Move cursor.
	directionBlock is a one argument Block that computes the new Position from a given one.
	specialBlock is a one argumentBlock that computes the new position from a given one under the alternate semantics.
	Note that directionBlock always is evaluated first."
	| shift indices newPosition |
	self morph manageCursor.
	shift := aKeyboardEvent shiftPressed.
	indices := self setIndices: shift forward: forward.
	newPosition := directionBlock value: (indices at: #moving).
	(aKeyboardEvent commandKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [newPosition := specialBlock value: newPosition].
	shift
		ifTrue: [self selectMark: (indices at: #fixed) point: newPosition - 1]
		ifFalse: [self selectAt: newPosition]
]

{ #category : #private }
Editor >> nextNonBlank: position [

"if current position is not separator, advance by 1, otherwise advance up until non-separator char found or new line"
	
	| string index |
	string := self string.
	index := position.
	index >= string size ifTrue: [ ^ string size+1 ].
	
	(string at: index) isSeparator ifFalse: [ ^ index + 1 ].
	index := index + 1.
	[ index <= string size and: [(string at: index) isSeparator and: [(string at:index) ~= Character cr]]]
		whileTrue: [index := index + 1].
	^ index
]

{ #category : #private }
Editor >> nextWord: position [
	| string index |
	string := self string.
	index := position.
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric]]
		whileTrue: [index := index + 1].
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric not]]
		whileTrue: [index := index + 1].
	^ index
]

{ #category : #'editing keys' }
Editor >> noop: aKeyboardEvent [ 
	"Unimplemented keyboard command; just ignore it."

	^ true
]

{ #category : #'typing/selecting keys' }
Editor >> normalCharacter: aKeyboardEvent [ 
	"A nonspecial character is to be added to the stream of characters."

	self addString: aKeyboardEvent keyCharacter asString.
	^false
]

{ #category : #'menu messages' }
Editor >> paste [
	"Paste the text from the shared buffer over the current selection and 
	redisplay if necessary.  Undoer & Redoer: undoAndReselect."

	self replace: self selectionInterval with: self clipboardText and:
		[self selectAt: self pointIndex]
]

{ #category : #'editing keys' }
Editor >> paste: aKeyboardEvent [ 
	"Replace the current text selection by the text in the shared buffer."

	self closeTypeIn.
	self paste.
	^true
]

{ #category : #private }
Editor >> previousNonBlank: position [
	"decrement position up until any non-blank character found, or end of line"
	| string index |
	position <= 1 ifTrue: [ ^ 1 ].
	
	string := self string.
	index := position.
	index := index -1.
	(string at: index) isSeparator ifFalse: [ ^ index ]. 

	[ index > 1 and: [(string at: index-1) isSeparator and: [(string at:index-1) ~= Character cr]]]
		whileTrue: [index := index - 1].
	^ index
]

{ #category : #private }
Editor >> previousWord: position [
	| string index |
	string := self string.
	index := position.
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric not]]
		whileTrue: [index := index - 1].
	[(index between: 1 and: string size) and: [(string at: index) isAlphaNumeric]]
		whileTrue: [index := index - 1].
	^ index + 1
]

{ #category : #'current selection' }
Editor >> select [
	"If the text selection is visible on the screen, reverse its highlight."
	^'Not doing anything'
	"screw this logic
	selectionShowing ifFalse: [self reverseSelection]"
]

{ #category : #'typing/selecting keys' }
Editor >> selectAll [

	self selectFrom: 1 to: self string size
]

{ #category : #'typing/selecting keys' }
Editor >> selectAll: aKeyboardEvent [ 
	"select everything"

	self closeTypeIn.
	self selectFrom: 1 to: self string size.
	^ true
]

{ #category : #'new selection' }
Editor >> selectAt: characterIndex [ 
	"Place the caret before the character at characterIndex.
	 Be sure it is in view."

	self selectFrom: characterIndex to: characterIndex - 1
]

{ #category : #'new selection' }
Editor >> selectInterval: anInterval [
	"Select the specified characters inclusive.
	 Be sure the selection is in view."

	self selectFrom: anInterval first to: anInterval last
]

{ #category : #'new selection' }
Editor >> selectInvisiblyAt: characterIndex [ 
	"Place the caret before the character at characterIndex.
	 Be sure it is in view but vithout any further action."
	
	self selectInvisiblyFrom: characterIndex to:  characterIndex - 1

]

{ #category : #'new selection' }
Editor >> selectInvisiblyFrom: start to: stop [
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: start; pointIndex: stop + 1
]

{ #category : #'new selection' }
Editor >> selectInvisiblyMark: mark point: point [
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: mark; pointIndex: point + 1
]

{ #category : #'new selection' }
Editor >> selectMark: mark point: point [
	"Select the specified characters inclusive.
	 Be sure the selection is in view."

	(mark =  self markIndex and: [point + 1 = self pointIndex]) ifFalse:
		[self deselect.
		self selectInvisiblyMark: mark point: point]
]

{ #category : #'new selection' }
Editor >> selectWord [
	"Select delimited text or word--the result of double-clicking."

	| openDelimiter closeDelimiter direction match level leftDelimiters rightDelimiters
	string here hereChar start stop |
	string := self string.
	here := self pointIndex.
	(here between: 2 and: string size)
		ifFalse: ["if at beginning or end, select entire string"
			^self selectFrom: 1 to: string size].
	leftDelimiters := '([{<''"
'.
	rightDelimiters := ')]}>''"
'.
	openDelimiter := string at: here - 1.
	match := leftDelimiters indexOf: openDelimiter.
	match > 0
		ifTrue: 
			["delimiter is on left -- match to the right"
			start := here.
			direction := 1.
			here := here - 1.
			closeDelimiter := rightDelimiters at: match]
		ifFalse: 
			[openDelimiter := string at: here.
			match := rightDelimiters indexOf: openDelimiter.
			match > 0
				ifTrue: 
					["delimiter is on right -- match to the left"
					stop := here - 1.
					direction := -1.
					closeDelimiter := leftDelimiters at: match]
				ifFalse: ["no delimiters -- select a token"
					direction := -1]].
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: 
			[hereChar := string at: (here := here + direction).
			match = 0
				ifTrue: ["token scan goes left, then right"
					hereChar tokenish
						ifTrue: [here = 1
								ifTrue: 
									[start := 1.
									"go right if hit string start"
									direction := 1]]
						ifFalse: [direction < 0
								ifTrue: 
									[start := here + 1.
									"go right if hit non-token"
									direction := 1]
								ifFalse: [level := 0]]]
				ifFalse: ["bracket match just counts nesting level"
					hereChar = closeDelimiter
						ifTrue: [level := level - 1"leaving nest"]
						ifFalse: [hereChar = openDelimiter 
									ifTrue: [level := level + 1"entering deeper nest"]]]].

	level > 0 ifTrue: ["in case ran off string end"	here := here + direction].
	direction > 0
		ifTrue: [self selectFrom: start to: here - 1]
		ifFalse: [self selectFrom: here + 1 to: stop]
]

{ #category : #'nonediting/nontyping keys' }
Editor >> selectWord: aKeyboardEvent [

	self closeTypeIn.
	self selectWord.
	^ true
]

{ #category : #accessing-selection }
Editor >> selectionInterval [
	"Answer the interval that is currently selected."

	^self startIndex to: self stopIndex - 1 
]

{ #category : #private }
Editor >> setIndices: shiftPressed forward: forward [
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices := Dictionary new.
	self flag: 'to be reviewed'.
	(shiftPressed)
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices
]

{ #category : #accessing }
Editor >> theme [
	^ Smalltalk ui theme 
]

{ #category : #accessing-selection }
Editor >> unselect [
	self markIndex: self pointIndex
]

{ #category : #accessing }
Editor >> userHasEdited [
	"Note that my text is free of user edits."
	morph notNil ifTrue:[ morph hasUnacceptedEdits: true ]
]
