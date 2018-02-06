"
The editor built specifically for Smalltalk code
"
Class {
	#name : #SmalltalkEditor,
	#superclass : #TextEditor,
	#category : #'Text-Edition'
}

{ #category : #shortcuts }
SmalltalkEditor class >> buildSmalltalkEditorKeymappingsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #browseIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseShortcut
		do: [ :morph | morph browseIt ].
	(aBuilder shortcut: #doIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current doItShortcut
		do: [ :morph | morph doIt ].
	(aBuilder shortcut: #inspectIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current inspectItShortcut
		do: [ :morph | morph inspectIt ].
	(aBuilder shortcut: #implementorsOfIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseImplementorsShortcut
		do: [ :morph | morph implementorsOfIt ].
	(aBuilder shortcut: #sendersOfIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current browseSendersShortcut
		do: [ :morph | morph sendersOfIt ].
	(aBuilder shortcut: #formatCode)
		category: #SmalltalkEditor
		default: PharoShortcuts current formatCodeShortcut
		do: [ :morph | morph formatSourceCode ].
	(aBuilder shortcut: #printIt)
		category: #SmalltalkEditor
		default: PharoShortcuts current printItShortcut
		do: [ :morph | morph printIt ]
]

{ #category : #'keyboard shortcut tables' }
SmalltalkEditor class >> initializeCmdKeyShortcuts [
	"Initialize the (unshifted) command-key (or alt-key) shortcut table."

	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	"SmalltalkEditor initialize"

	| cmds |
	self initializeBasicCmdKeyShortcuts.
	
	cmds := #(
		"$b #browseIt: "
		$d #doIt: 
		"$i #inspectIt:" 
		"$z #undo:" 
		$j #redo: 
		$l #cancel: 
		$m #implementorsOfIt: 
		"$n #sendersOfIt:" 
		"$p #printIt: "
		$q #querySymbol: 
		"$s #save:" ).
	1 to: cmds size
		by: 2
		do: [:i | cmdActions at: (cmds at: i) asciiValue + 1 put: (cmds at: i + 1)].
 
]

{ #category : #'keyboard shortcut tables' }
SmalltalkEditor class >> initializeShiftCmdKeyShortcuts [ 
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"
	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 
	capitalized versions of the letters.
	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."

	"SmalltalkEditor initialize"

	| cmds |
 	self initializeBasicShiftCmdKeyShortcuts.
	
	cmds := #(
		$a	argAdvance:
		$d	debugIt:
		$e	methodStringsContainingIt:
		$f	displayIfFalse:
		$g	fileItIn:
		$i	exploreIt:
		$n	referencesToIt:
		$t	displayIfTrue:
		$v	pasteInitials:
		$w	methodNamesContainingIt:
	).
	1 to: cmds size by: 2 do: [ :i |
		shiftCmdActions at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).			"plain keys"
		shiftCmdActions at: ((cmds at: i) asciiValue - 32 + 1) put: (cmds at: i + 1).		"shifted keys"
		shiftCmdActions at: ((cmds at: i) asciiValue - 96 + 1) put: (cmds at: i + 1).		"ctrl keys"
	].
]

{ #category : #'menu declaration' }
SmalltalkEditor class >> shiftedYellowButtonMenu [
	^ (PragmaMenuBuilder 
			pragmaKeyword: self smalltalkEditorShiftedMenuKeyword
			model: nil) menu.

]

{ #category : #'menu declaration' }
SmalltalkEditor class >> smalltalkEditorMenuKeyword [
	^'smalltalkEditorMenu'

]

{ #category : #'menu declaration' }
SmalltalkEditor class >> smalltalkEditorMenuOn: aBuilder [
	"Specify the menu used when writing code. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'smalltalkEditorMenu'
		model: nil) menu popUpInWorld
	"

	<contextMenu>
	<smalltalkEditorMenu>
	aBuilder
		item: 'Suggestions';
		selector: #smartSuggestions;
		target: self;
		keyText: 't';
		order: 0.
	aBuilder items last withSeparatorAfter.
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Basic Inspect it' translated)
		keyText: 'I';
		selector: #basicInspectIt;
		help: nil;
	 	iconName: #smallInspectItIcon.
	(aBuilder item: #'Debug it' translated)
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		iconName: #smallDebugIcon.
	(aBuilder item: #'Profile it' translated)
		selector: #tallyIt;
		iconName: #smallDebugIcon;
		withSeparatorAfter.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		help: nil;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		iconName: #smallFindIcon.
	(aBuilder item: #'Code search...' translated)
		selector: #shiftedTextPaneMenuRequest;
		withSeparatorAfter.
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		help: nil;
		iconName: #smallRedoIcon.
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		help: nil;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		iconName: #smallCopyIcon.
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		help: nil;
		iconName: #smallCutIcon.
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		help: nil;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		help: nil;
		iconName: #smallCopyIcon;
		withSeparatorAfter.
	(aBuilder item: #Accept translated)
		keyText: 's';
		selector: #accept;
		help: nil;
		iconName: #smallOkIcon.
	(aBuilder item: #Cancel translated)
		keyText: 'l';
		selector: #cancel;
		help: nil;
		iconName: #smallCancelIcon
]

{ #category : #'menu declaration' }
SmalltalkEditor class >> smalltalkEditorShiftedMenuKeyword [
	^ 'smalltalkEditorShiftedMenu'

]

{ #category : #'menu declaration' }
SmalltalkEditor class >> smalltalkEditorShiftedMenuOn: aBuilder [ 
	"Specify the menu used when writing code. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'smalltalkEditorShiftedMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<smalltalkEditorShiftedMenu>

	(aBuilder item: #'browse it' translated) 
		keyText: 'b';
		selector: #browseIt.
	(aBuilder item: #'senders of it' translated) 
		keyText: 'n';
		selector: #sendersOfIt.
	(aBuilder item: #'implementors of it' translated) 
		keyText: 'm';
		selector: #implementorsOfIt.
	(aBuilder item: #'references to it' translated) 
		keyText: 'N';
		selector: #referencesToIt; 
		withSeparatorAfter.
	(aBuilder item: #'selectors containing it' translated) 
		keyText: 'W';
		selector: #methodNamesContainingIt.
	(aBuilder item: #'method literal strings with it' translated) 
		keyText: 'E';
		selector: #methodStringsContainingit.
	(aBuilder item: #'method source with it' translated) 
		selector: #methodSourceContainingIt; 
		withSeparatorAfter.
	(aBuilder item: #'class names containing it' translated) 
		selector: #classNamesContainingIt.
	(aBuilder item: #'class comments with it' translated) 
		selector: #classCommentsContainingIt.

]

{ #category : #'menu declaration' }
SmalltalkEditor class >> yellowButtonMenu [
	^ (PragmaMenuBuilder 
			pragmaKeyword: self smalltalkEditorMenuKeyword
			model: nil) menu.

]

{ #category : #'typing/selecting keys' }
SmalltalkEditor >> argAdvance: aKeyboardEvent [
	"Invoked by Ctrl-a.  Useful after Ctrl-q.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the caret after the space.  If none are found, place the
		caret at the end of the text.  Does not affect the undoability of the 
	 	previous command."

	| start |
	self closeTypeIn.
	start := self text findString: ': ' startingAt: self stopIndex.
	start = 0 ifTrue: [ start := self text size + 1].
	self selectAt: start + 2.
	^true
]

{ #category : #'menu messages' }
SmalltalkEditor >> browseClassFrom: aClassString [
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."
	
	| selectedClass |
	self lineSelectAndEmptyCheck: [^ self].
	selectedClass := model systemNavigation 
				classFromPattern: aClassString
				withCaption: 'choose a class to browse...'.
				
	selectedClass isNil ifTrue: [^ self internalCallToImplementorsOf: aClassString ].
	
	Smalltalk tools browser openOnClass: selectedClass .
]

{ #category : #'menu messages' }
SmalltalkEditor >> browseClassFromIt [
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."
	
	self lineSelectAndEmptyCheck: [^ self].
	self browseClassFrom: (self selection string copyWithoutAll: CharacterSet crlf)
]

{ #category : #'menu messages' }
SmalltalkEditor >> browseIt [
	"Launch a browser for the current selection, if appropriate"
	^ self browseClassFromIt

]

{ #category : #'editing keys' }
SmalltalkEditor >> browseIt: t1 [ 
	self browseIt.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> classCommentsContainingIt [
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	model systemNavigation  browseClassCommentsWithString: self selection string matchCase: false. 
]

{ #category : #'menu messages' }
SmalltalkEditor >> classNamesContainingIt [
	"Open a browser on classes whose names contain the selected string"

	self lineSelectAndEmptyCheck: [^self].
	model systemNavigation 
		browseClassesWithNamesContaining: self selection string
		caseSensitive: Sensor shiftPressed
]

{ #category : #private }
SmalltalkEditor >> codeCompletionAround:  aBlock textMorph:  aTextMorph keyStroke: anEvent [
	"I'm a editor for Smalltalk, so, ok do completion around"
	Smalltalk tools codeCompletionAround: aBlock textMorph: aTextMorph keyStroke: anEvent
]

{ #category : #'do-its' }
SmalltalkEditor >> compileSelectionFor: anObject in: evalContext [
	| methodNode  |

	methodNode :=  self class compiler
		source: self selectionAsStream;
		class: anObject class;
		context: evalContext;
		requestor: self morph;
		noPattern: true;
		failBlock:  [ ^nil ];
		translate.
			
	^methodNode generateWithSource.
	
]

{ #category : #'do-its' }
SmalltalkEditor >> debug: aCompiledMethod receiver: anObject in: evalContext [
	| guineaPig context debugSession |
	guineaPig := [ aCompiledMethod
		valueWithReceiver: anObject
		arguments: (evalContext ifNil: [ #() ] ifNotNil: [ {evalContext} ]) ]
		newProcess.
	context := guineaPig suspendedContext.
	
	debugSession := guineaPig newDebugSessionNamed: 'debug it' startedAt: context.
	debugSession stepIntoUntil: [:currentContext | 
		currentContext method == aCompiledMethod ].
	
	Smalltalk tools debugger openOn: debugSession withFullView: true.
]

{ #category : #'do-its' }
SmalltalkEditor >> debugIt [

	| method receiver context |
	(model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := model doItReceiver.
			context := model doItContext]
		ifFalse:
			[receiver := context := nil].
	self lineSelectAndEmptyCheck: [^self].
	method := self compileSelectionFor: receiver in: context.
	method notNil ifTrue:
		[self debug: method receiver: receiver in: context].
]

{ #category : #'editing keys' }
SmalltalkEditor >> debugIt: aKeyboardEvent [
	"Evaluate the current selection in the Debugger."
	self debugIt.
	^ true
]

{ #category : #'typing/selecting keys' }
SmalltalkEditor >> displayIfFalse: aKeyboardEvent [
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	ctrl-f."

	self addString: 'ifFalse:'.
	^false
]

{ #category : #'typing/selecting keys' }
SmalltalkEditor >> displayIfTrue: aKeyboardEvent [
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	ctrl-t."

	self addString: 'ifTrue:'.
	^false
]

{ #category : #'do-its' }
SmalltalkEditor >> doIt [
	"Set the context to include pool vars of the model.  Then evaluate."
	^ self evaluateSelection
]

{ #category : #'editing keys' }
SmalltalkEditor >> doIt: aKeyboardEvent [
	"Called when user hits cmd-d.  Select the current line, if relevant, then evaluate and execute.  2/1/96 sw.
	2/29/96 sw: don't call selectLine; it's done by doIt now"	
	self doIt.
	^ true
]

{ #category : #accessing }
SmalltalkEditor >> editingStateClass [
	^ SmalltalkEditingState
]

{ #category : #'do-its' }
SmalltalkEditor >> evaluateSelection [
	"Treat the current selection as an expression; evaluate it and return the result"
	
	^self evaluateSelectionAndDo: [:result | result]
]

{ #category : #'do-its' }
SmalltalkEditor >> evaluateSelectionAndDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |

	self lineSelectAndEmptyCheck: [^ ''].
	(model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := model doItReceiver.
				ctxt := model doItContext]
		ifFalse: [ rcvr := ctxt := nil].
	result := rcvr class compiler
			source: self selectionForDoitAsStream;
			context: ctxt;
			receiver: rcvr;
			requestor: self morph  "don't set it to self.. The receiver can be destroyed as a result of evaluateSelection";
			failBlock:  [self morph flash. ^ nil];
			evaluate.		
			
	^ aBlock value: result
]

{ #category : #'do-its' }
SmalltalkEditor >> exploreIt [
	self evaluateSelectionAndDo: [:result | result basicInspect]
]

{ #category : #'editing keys' }
SmalltalkEditor >> exploreIt: aKeyboardEvent [
	"Explore the selection -- invoked via cmd-shift-I.  If there is no current selection, use the current line."
	self exploreIt.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> fileItIn [
	"Make a Stream on the text selection and fileIn it.
	 Moved here from FileController; this function can be useful from any text window that shows stuff in chunk format"

	| selection |
	selection := self selection.
	CodeImporter evaluateReadStream: (ReadWriteStream on: selection string from: 1 to: selection size).
	self inform: 'Code imported'.

]

{ #category : #'editing keys' }
SmalltalkEditor >> fileItIn: aKeyboardEvent [
	"File in the selection; invoked via a keyboard shortcut, -- for now, cmd-shift-G."
	self fileItIn.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> implementorsOf: aSelector [
	"Open an implementors browser on the given selector"

	(model systemNavigation allImplementorsOf: aSelector) isEmpty
			ifTrue: [self internalCallToBrowse: aSelector ]
			ifFalse: [ model systemNavigation browseAllImplementorsOf: aSelector]
]

{ #category : #'menu messages' }
SmalltalkEditor >> implementorsOfIt [
	"Open an implementors browser on the selected selector"

	| aSelector |
	self lineSelectAndEmptyCheck: [^ self].
	(aSelector := self selectedSelector) == nil ifTrue: [^ morph flash].
	self implementorsOf: aSelector
]

{ #category : #'editing keys' }
SmalltalkEditor >> implementorsOfIt: aKeyboardEvent [
	"Triggered by Cmd-m; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"
	self implementorsOfIt.
	^ true
]

{ #category : #'do-its' }
SmalltalkEditor >> inspectIt [
	self evaluateSelectionAndDo: [:result | result inspect]
]

{ #category : #'editing keys' }
SmalltalkEditor >> inspectIt: aKeyboardEvent [
	"Inspect the selection -- invoked via cmd-i.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine; it's done by inspectIt now"
	self inspectIt.
	^ true
]

{ #category : #shortcuts }
SmalltalkEditor >> installKeymappingsOn: aMorph [
	super installKeymappingsOn: aMorph.
	aMorph attachKeymapCategory: #SmalltalkEditor
]

{ #category : #'menu messages' }
SmalltalkEditor >> internalCallToBrowse: aSelector [
	"Launch a browser for the class indicated by the given selector. 
	If multiple classes matching the selection exist, let the user choose among them."
	| aClass |
	aClass := model systemNavigation 
				classFromPattern: aSelector
				withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ self].
	Smalltalk tools browser openOnClass: aClass.
]

{ #category : #'menu messages' }
SmalltalkEditor >> internalCallToBrowseIt [
	"Launch a browser for the class indicated by the current selection. 
	If multiple classes matching the selection exist, let the user choose among them."
 
	self lineSelectAndEmptyCheck: [^ self].
	self internalCallToBrowse: (self selection string copyWithoutAll: CharacterSet crlf)
]

{ #category : #'menu messages' }
SmalltalkEditor >> internalCallToImplementorsOf: aSelector [
	"Open an implementors browser on the given selector"
	
	 (model systemNavigation allImplementorsOf: aSelector) isEmpty
				ifFalse: [ model systemNavigation browseAllImplementorsOf: aSelector]
			
	
]

{ #category : #'menu messages' }
SmalltalkEditor >> internalCallToImplementorsOfIt [
	"Open an implementors browser on the selected selector"

	| aSelector |
	self lineSelectAndEmptyCheck: [^ self].
	(aSelector := self selectedSelector) == nil ifTrue: [^ morph flash].
	self internalCallToImplementorsOf: aSelector.
			
	
]

{ #category : #testing }
SmalltalkEditor >> isSmalltalkEditor [
	^ true 
]

{ #category : #'menu messages' }
SmalltalkEditor >> methodNamesContainingIt [
	"Open a browser on methods names containing the selected string"

	self lineSelectAndEmptyCheck: [ ^ self ].
	Cursor wait showWhile: [ 
		model systemNavigation  browseMethodsWhoseNamesContain: self selection string trimBoth ]
	
]

{ #category : #'editing keys' }
SmalltalkEditor >> methodNamesContainingIt: aKeyboardEvent [
	"Browse methods whose selectors containing the selection in their names"
	self methodNamesContainingIt.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> methodSourceContainingIt [
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source)."
	self lineSelectAndEmptyCheck: [^ self].
	model systemNavigation  browseMethodsWithSourceString: self selection string matchCase: false.
]

{ #category : #'editing keys' }
SmalltalkEditor >> methodStringsContainingIt: aKeyboardEvent [
	"Invoked from cmd-E -- open a browser on all methods holding string constants containing it."
	self methodStringsContainingit.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> methodStringsContainingit [
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	model systemNavigation  browseMethodsWithString: self selection string matchCase: false.

]

{ #category : #private }
SmalltalkEditor >> modelCurrentSelectedClass [
	" ugly dispatch, but current Browser protocol names aren't really cool "
	" for Nautilus ( and other tools), selectedClass sounds better, but I think it shouldn't be hardcoded "
	" IT'S A TEMP SOLUTION -- BenjaminVanRyseghem 14/04/11"

	" From now, all tools should use #selectedBehavior, waiting for a better solution - Thx Nice "
	(model respondsTo: #selectedBehavior)
		ifTrue: [^ model selectedBehavior ].
	
	" old selectors in order of uses "
	(model respondsTo: #selectedClass)
		ifTrue: [^ model selectedClass ].
	(model respondsTo: #selectedClassOrMetaClass)
		ifTrue: [ ^ model selectedClassOrMetaClass ].
	
		
	^ nil
]

{ #category : #'new selection' }
SmalltalkEditor >> nextTokenFrom: start direction: dir [
	"simple token-finder for compiler automated corrections"
	| loc str |
	loc := start + dir.
	str := self string.
	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]
		whileTrue: [loc := loc + dir].
	^ loc
]

{ #category : #'new selection' }
SmalltalkEditor >> notify: aString at: anInteger in: aStream [ 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."

	| pos |
	pos := self selectionInterval notEmpty
		ifTrue: [self startIndex + anInteger - 1 ]
		ifFalse: [anInteger].
	self insertAndSelect: aString at: (pos max: 1)
]

{ #category : #shortcuts }
SmalltalkEditor >> on: target keyEvent: event do: shortcutActionBlock [
	"Setup and break-up to be able to do a shortcut."

	target textMorph
		handleInteraction: [ target textMorph editor handleEditionAction: shortcutActionBlock fromKeyboardEvent: event ].
	target textChanged.
	target scrollSelectionIntoView
]

{ #category : #'editing keys' }
SmalltalkEditor >> pasteInitials: aKeyboardEvent [ 
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."
	self closeTypeIn.
	self replace: self selectionInterval with: (Text fromString: Author changeStamp) and: [self selectAt: self stopIndex].
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> prettyPrint [
	"Reformat the contents of the receiver's view (a Browser)."

	| selectedClass newText |
	model selectedMessageName ifNil: [^ morph flash].
	selectedClass := self modelCurrentSelectedClass.
	newText := selectedClass compiler
		source: self text;
		class: selectedClass;
		requestor: self;
		format.
	newText ifNotNil:
		[ self selectInvisiblyFrom: 1 to: self paragraph text size.
		self replaceSelectionWith: newText.
		self selectAt: 1]
]

{ #category : #'do-its' }
SmalltalkEditor >> printIt [
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	| printString |
	self
		evaluateSelectionAndDo: [ :result | 
			printString := [ result printString ]
				on: Error
				do: [ '<error in printString: try ''Inspect it'' to debug>' ].
			self afterSelectionInsertAndSelect: printString ]
]

{ #category : #'editing keys' }
SmalltalkEditor >> printIt: aKeyboardEvent [
	"Print the results of evaluting the selection -- invoked via cmd-p.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine now, since it's called by doIt"
	self printIt.
	^ true
]

{ #category : #'typing/selecting keys' }
SmalltalkEditor >> querySymbol: aKeyboardEvent [
	"Invoked by Ctrl-q to query the Symbol table and display alternate symbols.
	 See comment in completeSymbol:lastOffering: for details."

	self closeTypeIn.
	self editingState closeSelectorChooser.
	self editingState querySymbolFrom: morph.
	^true
]

{ #category : #'menu messages' }
SmalltalkEditor >> referencesTo: aVariableOrClassName [
	"Open a references browser on the given symbol"
	| env ref |

	" ugly dispatch, but current Browser protocol names aren't really cool "
	env := self modelCurrentSelectedClass ifNil: [ Smalltalk globals ].

	env isBehavior ifTrue: [
		(env instVarIndexFor: aVariableOrClassName) > 0 ifTrue: [
		 ^ self systemNavigation browseAllAccessesTo: aVariableOrClassName from: env]].

	ref:= (env bindingOf: aVariableOrClassName) ifNil: [ ^ self ].
	
	self systemNavigation browseAllReferencesTo: ref
]

{ #category : #'menu messages' }
SmalltalkEditor >> referencesToIt [
	"Open a references browser on the selected symbol"

	self selectLine.
	
	self selectedSymbol 
		ifNil: [ morph flash ]
		ifNotNil: [ :symbol | self referencesTo: symbol ]

]

{ #category : #'editing keys' }
SmalltalkEditor >> referencesToIt: aKeyboardEvent [
	"Triggered by Cmd-N; browse references to the current selection"
	self referencesToIt.
	^ true
]

{ #category : #'editing keys' }
SmalltalkEditor >> save: aKeyboardEvent [
	"Submit the current text.  Equivalent to 'accept' 1/18/96 sw"
	self closeTypeIn.
	self accept.
	^ true
]

{ #category : #'new selection' }
SmalltalkEditor >> selectLine [
	"Make the receiver's selection, if it currently consists of an insertion point only, encompass the current line."
	| i left right s |
	self hasSelection ifTrue: [^self].
	s := self string.
	i := s encompassParagraph: self selectionInterval.
	left := i first.
	right := i last.
	[ left <= right and: [ (s at: left) = $" or: [(s at: left) isSeparator] ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ (s at: right) = $"  or: [(s at: right) isSeparator] ] ]
		whileTrue: [ right := right - 1 ].
	self selectInterval: (left to: right)
]

{ #category : #'new selection' }
SmalltalkEditor >> selectPrecedingIdentifier [
	"Invisibly select the identifier that ends at the end of the selection, if any."

	| string sep stop tok |
	tok := false.
	string := self string.
	stop := self stopIndex - 1.
	[stop > 0 and: [(string at: stop) isSeparator]] whileTrue: [stop := stop - 1].
	sep := stop.
	[sep > 0 and: [(string at: sep) tokenish]] whileTrue: [tok := true. sep := sep - 1].
	tok ifTrue: [self selectInvisiblyFrom: sep + 1 to: stop]
]

{ #category : #'menu messages' }
SmalltalkEditor >> selectedSelector [
    "Try to make a selector out of the current text selection"

    | node |
    node := RBParser parseFaultyExpression: self selection string.
    node isMessage
        ifTrue: [ ^ node selector ].
    node isSequence "sequence of error nodes, we take the first one"
        ifTrue: [ ^ node  statements first value asSymbol].
    ^ node name
]

{ #category : #'menu messages' }
SmalltalkEditor >> selectedSymbol [
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasCaret ifTrue: [^ nil].
	aString := self selection string copyWithoutAll:
		{Character space.  Character cr.  Character tab}.
	aString size = 0 ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil
]

{ #category : #'do-its' }
SmalltalkEditor >> selectionForDoitAsStream [
	"Answer a ReadStream on the text in the paragraph that is currently
	selected. "
	^ ReadWriteStream
		on: self string
		from: self startIndex 
		to: self stopIndex - 1
]

{ #category : #'do-its' }
SmalltalkEditor >> selectionString [
	"remove the initial and final quote marks, if present"
	"'''h''' withoutWrappingDoubleQuotes"
	| quote selection  |
	selection :=  self selection string trimBoth .
	selection size < 2 ifTrue: [ ^ selection ].
	quote := selection first.
	^ quote = $"
		ifTrue: [ selection copyFrom: 2 to: selection size - 1 ]
		ifFalse: [ selection ].
]

{ #category : #'menu messages' }
SmalltalkEditor >> sendersOf: selectedSelector [
	" opens a Browser on the given selector"
	self model systemNavigation browseAllSendersOf: selectedSelector
]

{ #category : #'menu messages' }
SmalltalkEditor >> sendersOfIt [
	"Open a senders browser on the selected selector"

	| selectedSelector syst |
	self lineSelectAndEmptyCheck: [^ self].
	selectedSelector  := self selectedSelector.
	selectedSelector isNil ifTrue: [^ morph flash].
	syst := self model systemNavigation.
	syst browseAllSendersOrUsersOf: selectedSelector
]

{ #category : #'editing keys' }
SmalltalkEditor >> sendersOfIt: aKeyboardEvent [
	"Triggered by Cmd-n; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"
	self sendersOfIt.
	^ true
]

{ #category : #'menu messages' }
SmalltalkEditor >> spawn [
	"Create and schedule a message browser for the code of the model's 
	selected message. Retain any edits that have not yet been accepted."
	| code |
	code := self string.
	self cancel.
	model spawn: code.
]

{ #category : #'editing keys' }
SmalltalkEditor >> spawnIt: aKeyboardEvent [
	"Triggered by Cmd-o; spawn a new code window, if it makes sense."
	self spawn.
	^ true
]

{ #category : #'do-its' }
SmalltalkEditor >> tallyIt [

	^ self tallySelection
]

{ #category : #'do-its' }
SmalltalkEditor >> tallySelection [
	"Treat the current selection as an expression; evaluate and tally it."
	| receiver context v  compiledMethod profiler |
	(model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := model doItReceiver.
			context := model doItContext]
		ifFalse:
			[receiver := context := nil].
	self lineSelectAndEmptyCheck: [ ^ self ].
	compiledMethod := self compileSelectionFor: receiver in: context.
	compiledMethod ifNil: [^ self].
	profiler := TimeProfiler spyOn: [ v:= compiledMethod valueWithReceiver: receiver arguments: #()].
	profiler showResult: v.
	

]

{ #category : #shortcuts }
SmalltalkEditor >> toggleCommentOnSelectionOrLine [
	| originalHasSelection selection selectionInterval originalSelectionInterval quote    modifiedSelection newSelectionLastPosition numberOfDoubleQuoteBeforeCaret |
				
	selectionInterval := originalSelectionInterval := self selectionInterval.
	originalHasSelection := self hasSelection.
			
	originalHasSelection ifFalse: [
		selectionInterval := self string encompassLine: originalSelectionInterval.	
		self selectInvisiblyFrom: selectionInterval first to: selectionInterval last.
	].
					
	selection := self string copyFrom: selectionInterval first to: selectionInterval last.
		
	numberOfDoubleQuoteBeforeCaret := 0.
	selection withIndexDo: [ :char :i | 
		((i + selectionInterval first) < originalSelectionInterval first and: [ char = $" ]) ifTrue: [ 
		numberOfDoubleQuoteBeforeCaret := numberOfDoubleQuoteBeforeCaret  + 1
	]].
				
	quote := selection trimLeft first.
	quote = $" ifTrue: [ 
		modifiedSelection := selection asUncommentedCode.
		numberOfDoubleQuoteBeforeCaret := (((numberOfDoubleQuoteBeforeCaret - 1) // 2) + 1) negated.
		newSelectionLastPosition := (selection size - modifiedSelection size) negated.
	] ifFalse: [ 
		modifiedSelection := selection asComment.
		numberOfDoubleQuoteBeforeCaret := numberOfDoubleQuoteBeforeCaret + 1.
		newSelectionLastPosition := modifiedSelection size - selection size.
	].
			
	self replaceSelectionWith: modifiedSelection.
		
	originalHasSelection 
		ifFalse: [
			self selectAt: (originalSelectionInterval first + numberOfDoubleQuoteBeforeCaret).
		] ifTrue: [ 
			self selectInterval: ((originalSelectionInterval first) to: (originalSelectionInterval last + newSelectionLastPosition))
		]
]
