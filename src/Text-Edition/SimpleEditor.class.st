"
See comment in Editor
"
Class {
	#name : #SimpleEditor,
	#superclass : #Editor,
	#instVars : [
		'string',
		'pointIndex',
		'markIndex',
		'beginTypeInIndex'
	],
	#classVars : [
		'CmdActions',
		'ShiftCmdActions'
	],
	#category : 'Text-Edition'
}

{ #category : #initialization }
SimpleEditor class >> initialize [ 
	"Initialize the keyboard shortcut maps and the shared buffers
	 for copying text across views and managing again and undo.
	"

	self 
		initializeCmdKeyShortcuts;
		initializeShiftCmdKeyShortcuts
]

{ #category : #'private - initialization' }
SimpleEditor class >> initializeCmdKeyShortcuts [
	"Initialize the (unshifted) command-key (or alt-key) shortcut table."

	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	"
	SimpleEditor initialize
	"

	| cmdMap cmds |
	cmdMap := Array new: 256 withAll: #noop:.	"use temp in case of a crash"
	"cmdMap at: 1 + 1 put: #cursorHome:."	"home key"
	"cmdMap at: 4 + 1 put: #cursorEnd:."	"end key"
	cmdMap at: 8 + 1 put: #backspace:.	"ctrl-H or delete key"
	cmdMap at: 27 + 1 put: #escape:.	"left arrow key"
	cmdMap at: 28 + 1 put: #cursorLeft:.	"left arrow key"
	cmdMap at: 29 + 1 put: #cursorRight:.	"right arrow key"
	cmdMap at: 32 + 1 put: #selectWord:.	"space bar key"
	cmdMap at: 127 + 1 put: #forwardDelete:.	"del key"
	
	cmds := #("$a #selectAll: $c #copySelection: $v #paste:" $w #backWord: "$x #cut:" ).
	1 to: cmds size
		by: 2
		do: [:i | cmdMap at: (cmds at: i) asciiValue + 1 put: (cmds at: i + 1)].
	CmdActions := cmdMap
]

{ #category : #'private - initialization' }
SimpleEditor class >> initializeShiftCmdKeyShortcuts [ 
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"
	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 
	capitalized versions of the letters.
	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."

	| cmdMap |

	"shift-command and control shortcuts"
	cmdMap := Array new: 256 withAll: #noop:.  "use temp in case of a crash"
	"cmdMap at: ( 1 + 1) put: #cursorHome:."			"home key"
	"cmdMap at: ( 4 + 1) put: #cursorEnd:."			"end key"
	cmdMap at: ( 8 + 1) put: #forwardDelete:.		"ctrl-H or delete key"
	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"
	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"
	cmdMap at: (27 + 1) put: #escape.					"escape"				
	cmdMap at: (32 + 1) put: #selectWord:.			"space bar key"
	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"

	#(
		$x	makeLowercase:
		$y	makeUppercase:
		$z	makeCapitalized:
	) pairsDo: [ :char :symbol|
		cmdMap at: char asciiValue + 1 put: symbol.
		cmdMap at: char asciiValue + 1 put: symbol.		"plain keys"
		cmdMap at: char asciiValue - 32 + 1 put:symbol.		"shifted keys"
		cmdMap at: char asciiValue - 96 + 1 put: symbol.		"ctrl keys"
	].
	ShiftCmdActions := cmdMap
]

{ #category : #'typing - support' }
SimpleEditor >> addString: aString [
	"Think of a better name"

	self zapSelectionWith: aString
]

{ #category : #'typing - support' }
SimpleEditor >> backTo: startIndex [
	"During typing, backspace to startIndex.  Deleted characters fall into three
	 clusters, from left to right in the text: (1) preexisting characters that were
	 backed over; (2) newly typed characters that were backed over; 
	(3) preexisting characters that
	 were highlighted before typing began.  If typing has not yet been opened,
	 open it and watch for the first and third cluster.  If typing has been opened,
	 watch for the first and second cluster.  
	 Delete all the clusters.  The code is shorter than the comment."

	| saveLimit |
	saveLimit := beginTypeInIndex
		ifNil: [self openTypeIn. self stopIndex]
		ifNotNil: [self startOfTyping].
	self markIndex: startIndex.
	startIndex < saveLimit ifTrue: [
		beginTypeInIndex := self startIndex.].
	self zapSelectionWith: ''.
	self unselect
]

{ #category : #'initialize - release' }
SimpleEditor >> changeString: aString [
	"Install aString as the one to be edited by the receiver."

	string := aString.
	self resetState
]

{ #category : #'typing - support' }
SimpleEditor >> closeTypeIn [
	"See comment in openTypeIn.  It is important to call closeTypeIn before executing
	 any non-typing key, making a new selection, etc.  It is called automatically for
	 menu commands."

	beginTypeInIndex := nil
]

{ #category : #'menu messages' }
SimpleEditor >> copySelection [
	"Copy the current selection and store it in the paste buffer, unless a caret.  Undoer & Redoer: undoCutCopy"

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection
]

{ #category : #'nonediting - nontyping keys' }
SimpleEditor >> cursorEnd: aKeyboardEvent [

	"Private - Move cursor end of current line."
	self closeTypeIn.
	self
		moveCursor:
			[ :position | 
				string
					indexOf: Character cr
					startingAt: position
					ifAbsent:[string size + 1]]
		forward: true
		specialBlock: [:dummy | string size + 1]
		event: aKeyboardEvent.
	^true
]

{ #category : #'nonediting - nontyping keys' }
SimpleEditor >> cursorHome: aKeyboardEvent [ 

	"Private - Move cursor from position in current line to beginning of
	current line. If control key is pressed put cursor at beginning of text"

	self closeTypeIn.
	self
		moveCursor: [ :position |
				(string
					lastIndexOf: Character cr
					startingAt: position - 1
					ifAbsent:[0]) + 1]
		forward: false
		specialBlock: [:dummy | 1]
		event: aKeyboardEvent.
	^true
]

{ #category : #'menu messages' }
SimpleEditor >> cut [
	"Cut out the current selection and redisplay the paragraph if necessary.  Undoer & Redoer: undoCutCopy:"

	self lineSelectAndEmptyCheck: [^ self].
	self replaceSelectionWith: ''. 
	self clipboardTextPut: self selection
]

{ #category : #'typing - support' }
SimpleEditor >> dispatchOn: aKeyboardEvent [
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."

	| asciiValue honorCommandKeys |

	"We don't support multiple lines. Therefore, we don't process cr as a #normalCharacter:"
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue 
		ifTrue: [
			morph acceptOnCR
				ifTrue:[
					morph acceptContents .
					self closeTypeIn.
					^ true].
			aKeyboardEvent controlKeyPressed ifTrue: [ ^ self normalCharacter: aKeyboardEvent ].
			aKeyboardEvent shiftPressed ifTrue: [ ^ self lf: aKeyboardEvent ].
			aKeyboardEvent commandKeyPressed ifTrue: [ ^ self crlf: aKeyboardEvent ].
			^ self crWithIndent: aKeyboardEvent ].

	honorCommandKeys := self cmdKeysInText.

	"Special keys overwrite crtl+key combinations - at least on Windows. To resolve this
	conflict, assume that keys other than cursor keys aren't used together with Crtl." 
	((self class specialShiftCmdKeys includes: asciiValue) and: [ asciiValue < 27])
		ifTrue: [^ aKeyboardEvent controlKeyPressed
			ifTrue: [self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent]
			ifFalse: [self perform: (CmdActions at: asciiValue + 1) with: aKeyboardEvent]].

	"backspace, and escape keys (ascii 8 and 27) are command keys"
	((honorCommandKeys and: [aKeyboardEvent commandKeyPressed]) or: [self class specialShiftCmdKeys includes: asciiValue]) ifTrue: [
		^ aKeyboardEvent shiftPressed
			ifTrue: [
				self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent]
			ifFalse: [
				self perform: (CmdActions at: asciiValue + 1) with: aKeyboardEvent]].

	"the control key can be used to invoke shift-cmd shortcuts"
	(honorCommandKeys and: [aKeyboardEvent controlKeyPressed])
		ifTrue: [
			^ self perform: (ShiftCmdActions at: asciiValue + 1) with: aKeyboardEvent].

	^ self normalCharacter: aKeyboardEvent
]

{ #category : #'typing - support' }
SimpleEditor >> doneTyping [
	beginTypeInIndex := nil
]

{ #category : #'typing - selecting keys' }
SimpleEditor >> forwardDelete: aKeyboardEvent [
	"Delete forward over the next character.
	  Make Undo work on the whole type-in, not just the one char.
	wod 11/3/1998: If there was a selection use #zapSelectionWith: rather than #backspace: which was 'one off' in deleting the selection. Handling of things like undo or typeIn area were not fully considered."

	| startIndex stopIndex |
	startIndex := self markIndex.
	startIndex > string size ifTrue: [
		^ false].
	self hasSelection ifTrue: [
		"there was a selection"
		self zapSelectionWith: ''.
		^ false].
	"Null selection - do the delete forward"
	beginTypeInIndex isNil	"no previous typing.  openTypeIn"
		ifTrue: [self openTypeIn].
	stopIndex := startIndex.
	(aKeyboardEvent keyValue = 127 and: [ aKeyboardEvent shiftPressed ])
		ifTrue: [ stopIndex := (self nextWord: stopIndex) - 1 ].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: ''.
	self selectFrom: startIndex to: startIndex-1.
	^false
]

{ #category : #testing }
SimpleEditor >> hasCaret [
	^markIndex = pointIndex
]

{ #category : #initialization }
SimpleEditor >> initialize [
	"Initialize the state of the receiver. Subclasses should include 'super 
	initialize' when redefining this message to insure proper initialization."

	super initialize.
	self changeString: ''
]

{ #category : #testing }
SimpleEditor >> isAtEnd [
	^pointIndex = (string size + 1)
]

{ #category : #testing }
SimpleEditor >> isAtStart [
	^pointIndex = 1
]

{ #category : #testing }
SimpleEditor >> isSimpleEditor [ 
	^ true 
]

{ #category : #'editing keys' }
SimpleEditor >> makeCapitalized: aKeyboardEvent [
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	| prev |
	prev := $-.  "not a letter"
	self replaceSelectionWith:
		(self selection collect: [:c |
			prev := prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
	^ true
]

{ #category : #'editing keys' }
SimpleEditor >> makeLowercase: aKeyboardEvent [
	"Force the current selection to lowercase.  Triggered by Cmd-X."

	self replaceSelectionWith: (self selection asLowercase).
	^ true
]

{ #category : #'editing keys' }
SimpleEditor >> makeUppercase: aKeyboardEvent [
	"Force the current selection to uppercase.  Triggered by Cmd-Y."

	self replaceSelectionWith: (self selection asUppercase).
	^ true
]

{ #category : #'accessing - selection' }
SimpleEditor >> markIndex [
	^markIndex
]

{ #category : #'accessing - selection' }
SimpleEditor >> markIndex: anIndex [
	markIndex := anIndex min: string size + 1 max: 1
]

{ #category : #events }
SimpleEditor >> mouseDown: evt index: index [
	self closeTypeIn.
	evt shiftPressed
		ifFalse: [ self
				pointIndex: index;
				markIndex: index ]
		ifTrue: [ self mouseMove: evt ]
]

{ #category : #events }
SimpleEditor >> mouseMove: evt index: index [
	"Change the selection in response to moue-down drag"

	self pointIndex: index
]

{ #category : #'typing - support' }
SimpleEditor >> openTypeIn [
	"Set up beginTypeInIndex to keep track of the leftmost backspace."

	beginTypeInIndex ifNil: [
		beginTypeInIndex := self startIndex ]
]

{ #category : #'accessing - selection' }
SimpleEditor >> pointIndex [
	^ pointIndex
]

{ #category : #'accessing - selection' }
SimpleEditor >> pointIndex: anIndex [
	pointIndex := anIndex min: string size + 1 max: 1
]

{ #category : #'typing - support' }
SimpleEditor >> processKeyStroke: aKeyboardEvent [

	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters.  Undoer & Redoer: see closeTypeIn."

	(self dispatchOn: aKeyboardEvent) 
		ifTrue: [self doneTyping.
			^self].
	self openTypeIn.
	self unselect
]

{ #category : #accessing }
SimpleEditor >> replace: oldInterval with: newString and: selectingBlock [ 
	"Replace the text in oldInterval with newString and execute selectingBlock to establish the new selection."

	self selectionInterval = oldInterval ifFalse: [self selectInterval: oldInterval].
	self zapSelectionWith: newString.
	selectingBlock value
]

{ #category : #accessing }
SimpleEditor >> replaceSelectionWith: aString [
	"Replace the selection text by aText."

	self zapSelectionWith: aString
]

{ #category : #'initialize - release' }
SimpleEditor >> resetState [ 
	"Establish the initial conditions for editing the paragraph: place caret 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex := 1.	"Like pointBlock in EnhancedTextEditor"
	markIndex := 1.	"Like markIndex in EnhancedTextEditor"
	beginTypeInIndex := nil.	"Like beginTypeInBlock in EnhancedTextEditor"
	selectionShowing := false
]

{ #category : #'new selection' }
SimpleEditor >> selectFrom: start to: stop [
	"Select the specified characters inclusive."
	self selectInvisiblyFrom: start to: stop.
	self closeTypeIn
]

{ #category : #'new selection' }
SimpleEditor >> selectLine [
	"Make the receiver's selection, if it currently consists of an insertion point only, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectAll
]

{ #category : #'accessing - selection' }
SimpleEditor >> selection [

	^string copyFrom: self startIndex to: self stopIndex - 1 
]

{ #category : #'accessing - selection' }
SimpleEditor >> startIndex [
	^ pointIndex min: markIndex
]

{ #category : #'typing - support' }
SimpleEditor >> startOfTyping [
	^ beginTypeInIndex
]

{ #category : #'initialize - release' }
SimpleEditor >> stateArray [
	^ {	self selectionInterval.
		self startOfTyping}
]

{ #category : #'initialize - release' }
SimpleEditor >> stateArrayPut: stateArray [
	| sel |
	sel := stateArray at: 1.
	self selectFrom: sel first to: sel last.
	beginTypeInIndex := stateArray at: 2
]

{ #category : #'accessing - selection' }
SimpleEditor >> stopIndex [
	^ pointIndex max: markIndex
]

{ #category : #accessing }
SimpleEditor >> string [
	^string
]

{ #category : #'mvc compatibility' }
SimpleEditor >> zapSelectionWith: aString [

	| start stop |
	start := self startIndex.
	stop := self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string := string copyReplaceFrom: start to: stop-1 with: aString.
		self
			markIndex: start; 
			pointIndex: start + aString size.

		self userHasEdited  " -- note text now dirty" ]
]
