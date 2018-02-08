"
I am the editor for smalltalk code, I allow to browse, execute etc
"
Class {
	#name : #RubSmalltalkEditor,
	#superclass : #RubTextEditor,
	#instVars : [
		'selectorChooser',
		'notificationStrategy'
	],
	#category : #'Rubric-Editing-Code'
}

{ #category : #keymapping }
RubSmalltalkEditor class >> buildPatchShortcutsForDoItInWindowsOn: aBuilder [
	"this is because for some reason Ctrl+d means Ctrl+end on Windows"
	<keymap>
	(aBuilder shortcut: #doItPatchForWindows) 
		category: RubSmalltalkEditor name
		default: Character end ctrl win
		do: [ :target | target editor doIt: nil ]
		description: 'Do it'.
	
	(aBuilder shortcut: #debugItPatchForWindows) 
		category: RubSmalltalkEditor name
		default: Character end shift ctrl win
		do: [ :target | target editor debugIt: nil ]
		description: 'Debug it'
]

{ #category : #keymapping }
RubSmalltalkEditor class >> buildShortcutsOn: aBuilder [
	"We are defining the bindings twice because we want to support 
	both Cmd and meta for Windows and Linux. This should happen at least as long as in the development environment
	both of these are supported.
	
	We list both variations explicitly because we want to be able to see the action code when inspecting the morph.
	"

	<keymap>
	(aBuilder shortcut: #querySymbol)
		category: RubSmalltalkEditor name
		default: $q meta
		do: [ :target | target editor querySymbol: nil ]
		description: 'Query symbol'.
	
	(aBuilder shortcut: #browseIt)
		category: RubSmalltalkEditor name
		default: $b meta
		do: [ :target | target editor browseIt: nil ]
		description: 'Browse'.
	
	(aBuilder shortcut: #doIt)
		category: RubSmalltalkEditor name
		default: $d meta
		do: [ :target | target editor doIt: nil ]
		description: 'Do it'.
	
	(aBuilder shortcut: #inspectIt)
		category: RubSmalltalkEditor name
		default: $i meta
		do: [ :target | target editor inspectIt: nil ]
		description: 'Inspect it'.
	
	(aBuilder shortcut: #basicInspectIt)
		category: RubSmalltalkEditor name
		default: $i meta shift
		do: [ :target | target editor basicInspectIt ]
		description: 'Basic Inspect it'.
	
	(aBuilder shortcut: #implementorsOfIt)
		category: RubSmalltalkEditor name
		default: $m meta
		do: [ :target | target editor implementorsOfIt: nil ]
		description: 'Implementors of it'.
	
	(aBuilder shortcut: #senderOfIt)
		category: RubSmalltalkEditor name
		default: $n meta
		do: [ :target | target editor sendersOfIt: nil ]
		description: 'Senders of it'.
	
	(aBuilder shortcut: #printIt)
		category: RubSmalltalkEditor name
		default: $p meta
		do: [ :target | target editor printIt ]
		description: 'Print it'.
	
	(aBuilder shortcut: #debugIt)
		category: RubSmalltalkEditor name
		default: $d meta shift
		do: [ :target | target editor debugIt: nil ]
		description: 'Debug it'.
	
	(aBuilder shortcut: #referencesToIt)
		category: RubSmalltalkEditor name
		default: $n meta shift
		do: [ :target | target editor referencesToIt: nil ]
		description: 'References to it'.
	
	(aBuilder shortcut: #methodStringsContainingIt)
		category: RubSmalltalkEditor name
		default: $e meta shift
		do: [ :target | target editor methodStringsContainingIt: nil ]
		description: 'Method strings containing it'.
	
	(aBuilder shortcut: #format)
		category: RubSmalltalkEditor name
		default: PharoShortcuts current formatCodeShortcut
		do: [ :target | target formatMethodCode ]
]

{ #category : #accessing }
RubSmalltalkEditor class >> compilerFor: aClass [
	^ (aClass respondsTo: #compiler) 
		ifTrue: [aClass compiler]
		ifFalse: [ aClass class evaluatorClass new ]
]

{ #category : #NOcompletion }
RubSmalltalkEditor >> atCompletionPosition [
	"Return true if the cursor is at a possible completion position"
	| cursorPosition |
	cursorPosition := self startIndex.
	cursorPosition < 2 ifTrue: [ ^ false ].
	^ (self text at: cursorPosition - 1) isCompletionCharacter
]

{ #category : #'do-its' }
RubSmalltalkEditor >> basicInspectIt [
	self evaluateSelectionAndDo: [:result | result basicInspect]
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> bestNodeInTextArea [
	"Find the best node in the editor text area at the current pointer location"
	
	| node start stop |
	
	start := self textArea startIndex.
	stop := self textArea stopIndex.
	
	[node := RBParser parseMethod: self textArea string onError: [ 
		RBParser parseFaultyExpression: self textArea string]
	] on: Error do: [^nil].
	
	^node bestNodeFor: (start to: stop).
	
]

{ #category : #binding }
RubSmalltalkEditor >> bindingOf: aString [
	^ self textArea bindingOf: aString
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> browseClassFrom: aClassString [
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| selectedClass |
	"self lineSelectAndEmptyCheck: [ ^ self ]."
	(aClassString isNil or: [ aClassString isEmpty]) ifTrue: [ 
		textArea flash.
		^self ]. 
	
	selectedClass := self model systemNavigation 
		classFromPattern: aClassString 
		withCaption: 'choose a class to browse...'.
	selectedClass isNil
		ifTrue: [ ^ self internalCallToImplementorsOf: aClassString ].
	Smalltalk tools browser openOnClass: selectedClass
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> browseClassFromIt [
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."
	
	"self lineSelectAndEmptyCheck: [^ self]."
	self browseClassFrom: self findClassFromAST
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> browseIt [
	"Launch a browser for the current selection, if appropriate"
	^ self browseClassFromIt

]

{ #category : #'editing keys' }
RubSmalltalkEditor >> browseIt: t1 [ 

	self browseIt.
	^ true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> classCommentsContainingIt [
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	self model systemNavigation  browseClassCommentsWithString: self selection string matchCase: false. 
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> classNamesContainingIt [
	"Open a browser on classes whose names contain the selected string"

	self lineSelectAndEmptyCheck: [^self].
	self model systemNavigation 
		browseClassesWithNamesContaining: self selection string
		caseSensitive: Sensor shiftPressed
]

{ #category : #events }
RubSmalltalkEditor >> click: anEvent [ 	
	self selectorChooserClose.
	^ super click: anEvent 

]

{ #category : #'do-its' }
RubSmalltalkEditor >> compile: aStream for: anObject in: evalContext [
	| methodNode methodClass |
	methodClass := evalContext
		ifNil: [ anObject class ]
		ifNotNil: [ evalContext methodClass ].
	methodNode := self class compiler
		source: aStream;
		class: methodClass;
		context: evalContext;
		requestor: self morph;
		noPattern: true;
		failBlock: [ ^ nil ];
		translate.
	^ methodNode generateWithSource
]

{ #category : #'do-its' }
RubSmalltalkEditor >> compileSelectionFor: anObject in: evalContext [
	
	^ self compile: self selectionAsStream for: anObject in: evalContext
]

{ #category : #private }
RubSmalltalkEditor >> completionAround:  aBlock  keyStroke: anEvent [
	"I'm a editor for Smalltalk, so, ok do completion around"
	Smalltalk tools codeCompletionAround: aBlock textMorph: textArea keyStroke: anEvent
]

{ #category : #'new selection' }
RubSmalltalkEditor >> computeSelectionIntervalForCurrentLine [
	| i left right s quoteCounter|
	s := self string.
	i := s encompassParagraph: self selectionInterval.
	left := i first.
	right := i last.
	"we need to skip all text that is inside of quote pairs"
	quoteCounter := 0.
	[ left <= right and: [ (s at: left) = $"
			or: [ quoteCounter % 2 = 1
				or: [(s at: left) isSeparator] ] ] ]
		whileTrue: [ 
			(s at: left) = $" ifTrue: [ quoteCounter := quoteCounter + 1 ].
			left := left + 1 ].
	quoteCounter := 0.
	[ left <= right and: [ ((s at: right) = $"
			"we need to stop if quote is defined as character"
			and: [ right - 1 < 1 or: [ (s at: right - 1) ~= $$ ] ])
				or: [ quoteCounter % 2 = 1 or: [(s at: right) isSeparator] ] ] ]
		whileTrue: [
			(s at: right) = $" ifTrue: [ quoteCounter := quoteCounter + 1 ].
			right := right - 1 ].
	^ left to: right
]

{ #category : #private }
RubSmalltalkEditor >> computeSelectionIntervalFromCodeIn: aString at: anInterval [
	^ (RBParser parseExpression: (aString copyFrom: anInterval first to: anInterval last)) sourceInterval
]

{ #category : #private }
RubSmalltalkEditor >> computeSelectionIntervalFromCommentIn: aString at: anInterval [
	| comment commentInterval |
	comment := (RBScanner on: (aString copyFrom: anInterval first to: anInterval last) readStream) getComments.
	comment ifNil: [ ^ 0 to: -1 ].
	commentInterval := comment first.
	^ commentInterval first + 1 to: commentInterval last - 1
]

{ #category : #'do-its' }
RubSmalltalkEditor >> debug: aStream [

	| method receiver context |
	(self model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := self model doItReceiver.
			context := self model doItContext]
		ifFalse:
			[receiver := context := nil].
	method := self compile: aStream for: receiver in: context.
	
	method isReturnSpecial ifTrue:[ self inform:'Nothing to debug, the expression is optimized'. ^self.].
	
	method notNil ifTrue: [
		self debug: method receiver: receiver in: context ].
	
]

{ #category : #'do-its' }
RubSmalltalkEditor >> debug: aCompiledMethod receiver: anObject in: evalContext [
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
RubSmalltalkEditor >> debugHighlight [
	"Treat the current highlight as an expression; evaluate and debugg it in a new debugger."

	self highlightAndEmptyCheck: [ ^ self ].
	self debug: self highlightedTextAsStream
]

{ #category : #'do-its' }
RubSmalltalkEditor >> debugIt [

	^ self debugSelection
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> debugIt: aKeyboardEvent [
	"Evaluate the current selection in the Debugger."

	self debugIt.
	^ true
]

{ #category : #'do-its' }
RubSmalltalkEditor >> debugSelection [
	"Treat the current selection as an expression; evaluate and debugg it in a new debugger."
	
	self lineSelectAndEmptyCheck: [^self].
	self debug: self selectionAsStream
]

{ #category : #'typing/selecting keys' }
RubSmalltalkEditor >> displayIfFalse: aKeyboardEvent [
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	ctrl-f."

	self addString: 'ifFalse:'.
	^false
]

{ #category : #'typing/selecting keys' }
RubSmalltalkEditor >> displayIfTrue: aKeyboardEvent [
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	ctrl-t."

	self addString: 'ifTrue:'.
	^false
]

{ #category : #'do-its' }
RubSmalltalkEditor >> doIt [
	"Set the context to include pool vars of the model.  Then evaluate."
	^ self evaluateSelection
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> doIt: aKeyboardEvent [
	"Called when user hits cmd-d.  Select the current line, if relevant, then evaluate and execute.  2/1/96 sw.
	2/29/96 sw: don't call selectLine; it's done by doIt now"
	
	self doIt.
	^ true
]

{ #category : #'do-its' }
RubSmalltalkEditor >> evaluate: aStream andDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	| result rcvr ctxt |
	(self model respondsTo: #doItReceiver) 
		ifTrue: [ rcvr := self model doItReceiver.
				ctxt := self model doItContext]
		ifFalse: [ rcvr := ctxt := nil].
	result := rcvr class compiler
			source: aStream;
			context: ctxt;
			receiver: rcvr;
			requestor: self morph  "don't set it to self.. The receiver can be destroyed as a result of evaluateSelection";
			failBlock:  [self morph flash. ^ nil];
			evaluate.		
			
	^ aBlock value: result
]

{ #category : #'do-its' }
RubSmalltalkEditor >> evaluateSelection [
	"Treat the current selection as an expression; evaluate it and return the result"
	
	^self evaluateSelectionAndDo: [:result | result]
]

{ #category : #'do-its' }
RubSmalltalkEditor >> evaluateSelectionAndDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result.
	If no selection is present select the current line."

	self lineSelectAndEmptyCheck: [^ ''].
	^ self 
		evaluate: self selectionForDoitAsStream 
		andDo: aBlock
]

{ #category : #'do-its' }
RubSmalltalkEditor >> exploreIt [
	self evaluateSelectionAndDo: [:result | result inspect]
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> exploreIt: aKeyboardEvent [
	"Explore the selection -- invoked via cmd-shift-I.  If there is no current selection, use the current line."
	
	self exploreIt.
	^ true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> findClassFromAST [
	"Try to make a class name out of the current text selection"

	| node fallbackBlock className |		
	
	"Preserve original behavior - although could consider building AST from selection"
	self hasSelection ifTrue: [ 
		^(self selection string copyWithoutAll: CharacterSet crlf) trimBoth ].
	
	fallbackBlock := [^nil].

	node := self bestNodeInTextArea.
	
	node ifNil: [ 
		self selectLine.
		^ self hasSelection 
			ifTrue: [ (self selection string copyWithoutAll: CharacterSet crlf) trimBoth ]
			ifFalse: [ nil ] ].
	
	[node isVariable] whileFalse: [ 
	 	(node := node parent) ifNil: fallbackBlock ].
	
	className := node name.
	
	[ className first isUppercase ] whileFalse: [ 
		(className := className allButFirst) ifEmpty: fallbackBlock  ].
	
	^className
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> findSelectorFromAST [
	"Try to make a selector out of the current text selection"

	| node |
	node := self bestNodeInTextArea.
	(node isNil or: [node isSequence]) ifTrue: [
		self selectLine.
		^ RBParser parseFaultyExpression: self selection string.]. 
	node isMethod ifFalse: [ 
		[ node isMessage ] whileFalse: [ 
	 		(node := node parent) ]].
	^node 
]

{ #category : #private }
RubSmalltalkEditor >> focusChanged [
	super focusChanged.
	self selectorChooserFocusChange 
]

{ #category : #'do-its' }
RubSmalltalkEditor >> format [
	self model formatSourceCodeInView
]

{ #category : #'do-its' }
RubSmalltalkEditor >> highlightEvaluateAndDo: aBlock [
	"Treat the current selection as an expression; evaluate it and invoke aBlock with the result."
	
	self highlightAndEmptyCheck: [ ^ '' ].
	^ self 
		evaluate: self highlightedTextAsStream
		andDo: [:result | aBlock value: result]
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> implementorsOf: aSelector [
	"Open an implementors browser on the given selector"

	(self model systemNavigation allImplementorsOf: aSelector) isEmpty
		ifTrue: [ self internalCallToBrowse: aSelector ]
		ifFalse: [ self model interactionModel systemNavigation browseAllImplementorsOf: aSelector ]
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> implementorsOfIt [
	"Open an implementors browser on the selected selector"

	| aSelector |
	(aSelector := self selectedSelector) ifNil: [^ textArea flash].
	self implementorsOf: aSelector

]

{ #category : #'editing keys' }
RubSmalltalkEditor >> implementorsOfIt: aKeyboardEvent [
	"Triggered by Cmd-m; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"

	self implementorsOfIt.
	^ true
]

{ #category : #initialization }
RubSmalltalkEditor >> initialize [
	super initialize.
	notificationStrategy := RubTextInsertionStrategy new editor: self
]

{ #category : #keymapping }
RubSmalltalkEditor >> initializeShortcuts: aKMDispatcher [
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: RubSmalltalkEditor name
]

{ #category : #'do-its' }
RubSmalltalkEditor >> inspectIt [
	self evaluateSelectionAndDo: [:result | result inspect]
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> inspectIt: aKeyboardEvent [
	"Inspect the selection -- invoked via cmd-i.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine; it's done by inspectIt now"

	self inspectIt.
	^ true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> internalCallToBrowse: aSelector [
	"Launch a browser for the class indicated by the given selector. 
	If multiple classes matching the selection exist, let the user choose among them."
	| aClass |
	aClass := self model systemNavigation 
				classFromPattern: aSelector
				withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ self].
	Smalltalk tools browser openOnClass: aClass
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> internalCallToBrowseIt [
	"Launch a browser for the class indicated by the current selection. 
	If multiple classes matching the selection exist, let the user choose among them."

	self lineSelectAndEmptyCheck: [^ self].
	self internalCallToBrowse: (self selection string copyWithoutAll: CharacterSet crlf)
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> internalCallToImplementorsOf: aSelector [
	"Open an implementors browser on the given selector"
	
	 (self model systemNavigation allImplementorsOf: aSelector) isEmpty
				ifFalse: [ self model systemNavigation browseAllImplementorsOf: aSelector]
			
	
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> internalCallToImplementorsOfIt [
	"Open an implementors browser on the selected selector"

	| aSelector |
	self lineSelectAndEmptyCheck: [ ^ self ].
	(aSelector := self selectedSelector) isNil
		ifTrue: [ ^ textArea flash ].
	self internalCallToImplementorsOf: aSelector
]

{ #category : #testing }
RubSmalltalkEditor >> isSmalltalkEditor [
	^ true 
]

{ #category : #'typing support' }
RubSmalltalkEditor >> keystroke: aKeyboardEvent [
	self
		completionAround: [ 
			(self selectorChooserHandlesKeyboard: aKeyboardEvent)
				ifFalse: [ super keystroke: aKeyboardEvent ].
			self selectorChooserKeystroke: aKeyboardEvent ]
		keyStroke: aKeyboardEvent
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> methodCaseSensitiveStringsContainingit [
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	
	self model systemNavigation  browseMethodsWithString: self selection string matchCase: true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> methodNamesContainingIt [
	"Open a browser on methods names containing the selected string"

	self lineSelectAndEmptyCheck: [ ^ self ].
	Cursor wait
		showWhile: [ self model interactionModel systemNavigation
				browseMethodsWhoseNamesContain: self selection string trimBoth ]
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> methodNamesContainingIt: aKeyboardEvent [
	"Browse methods whose selectors containing the selection in their names"

	self methodNamesContainingIt.
	^ true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> methodSourceContainingIt [
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source)."

	self lineSelectAndEmptyCheck: [ ^ self ].
	self model interactionModel systemNavigation
		browseMethodsWithSourceString: self selection string
		matchCase: false
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> methodStringsContainingIt: aKeyboardEvent [
	"Invoked from cmd-E -- open a browser on all methods holding string constants containing it."

	self methodStringsContainingit.
	^ true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> methodStringsContainingit [
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [ ^ self ].
	self model interactionModel systemNavigation
		browseMethodsWithString: self selection string
		matchCase: false
]

{ #category : #private }
RubSmalltalkEditor >> modelCurrentSelectedClass [
	" ugly dispatch, but current Browser protocol names aren't really cool "
	" for Nautilus ( and other tools), selectedClass sounds better, but I think it shouldn't be hardcoded "
	" IT'S A TEMP SOLUTION -- BenjaminVanRyseghem 14/04/11"

	" From now, all tools should use #selectedBehavior, waiting for a better solution - Thx Nice "
	(self model interactionModel respondsTo: #selectedBehavior)
		ifTrue: [^ self model interactionModel selectedBehavior ].
	
	" old selectors in order of uses "
	(self model respondsTo: #selectedClass)
		ifTrue: [^ self model selectedClass ].
	(self model respondsTo: #selectedClassOrMetaClass)
		ifTrue: [ ^ self model selectedClassOrMetaClass ].
	
		
	^ nil
]

{ #category : #'new selection' }
RubSmalltalkEditor >> nextTokenFrom: start direction: dir [
	"simple token-finder for compiler automated corrections"
	| loc str |
	loc := start + dir.
	str := self string.
	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]
		whileTrue: [loc := loc + dir].
	^ loc
]

{ #category : #accessing }
RubSmalltalkEditor >> notificationStrategy [
	^ notificationStrategy
]

{ #category : #accessing }
RubSmalltalkEditor >> notificationStrategy: aStrategy [
	notificationStrategy := aStrategy.
	aStrategy editor: self
]

{ #category : #'new selection' }
RubSmalltalkEditor >> notify: aString at: anInteger in: aStream [ 
	^ self notificationStrategy 
		notify: aString 
		at: anInteger 
		in: aStream 

]

{ #category : #'editing keys' }
RubSmalltalkEditor >> pasteInitials: aKeyboardEvent [ 
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."

	self closeTypeIn.
	self replace: self selectionInterval with: (Text fromString: Author changeStamp) and: [self selectAt: self stopIndex].
	^ true
]

{ #category : #'do-its' }
RubSmalltalkEditor >> printIt [
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
RubSmalltalkEditor >> printIt: aKeyboardEvent [
	"Print the results of evaluting the selection -- invoked via cmd-p.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine now, since it's called by doIt"
	self printIt.
	^ true
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> querySymbol [
	self selectorChooseQuerySymbol 
]

{ #category : #'typing/selecting keys' }
RubSmalltalkEditor >> querySymbol: aKeyboardEvent [
	"Invoked by Ctrl-q to query the Symbol table and display alternate symbols.
	 See comment in completeSymbol:lastOffering: for details."

	self closeTypeIn.
	self selectorChooserClose.
	self querySymbol.
	^true
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> referencesTo: aVariableOrClassName [
	"Open a references browser on the given symbol"
	| env ref |

	" ugly dispatch, but current Browser protocol names aren't really cool "
	env := self modelCurrentSelectedClass ifNil: [ Smalltalk globals ].

	env isBehavior ifTrue: [
		(env instVarIndexFor: aVariableOrClassName) > 0 ifTrue: [
		 ^ self systemNavigation browseAllAccessesTo: aVariableOrClassName from: env]].

	ref:= (env bindingOf: aVariableOrClassName) ifNil: [ ^ self ].
	
	self systemNavigation browseAllSendersOf: ref
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> referencesToIt [
	"Open a references browser on the selected symbol"

	| aSymbol |
	self selectLine.
	(aSymbol := self selectedSymbol) isNil
		ifTrue: [ ^ textArea flash ].
	self referencesTo: aSymbol
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> referencesToIt: aKeyboardEvent [
	"Triggered by Cmd-N; browse references to the current selection"

	self referencesToIt.
	^ true
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> save: aKeyboardEvent [
	"Submit the current text.  Equivalent to 'accept' 1/18/96 sw"

	self closeTypeIn.
	self accept.
	^ true
]

{ #category : #'new selection' }
RubSmalltalkEditor >> selectPrecedingIdentifier [
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
RubSmalltalkEditor >> selectedSelector [
	"Try to make a selector out of the current text selection"

	| node |
	
	node := self hasSelection 
		ifTrue: [ RBParser parseFaultyExpression: self selection string. ]
		ifFalse: [ self findSelectorFromAST ].
	node nodesDo: [ :n | 
			n isMessage
				ifTrue: [ ^ n selector ].
			n isVariable
				ifTrue: [ ^ n name ].
			n isLiteralNode
				ifTrue: [ ^ n value ] ].
	"fall back"
	^ self selection string asSymbol
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> selectedSymbol [
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasCursor ifTrue: [^ nil].
	aString := self selection string copyWithoutAll:
		{Character space.  Character cr.  Character tab}.
	aString isEmpty ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil
]

{ #category : #'do-its' }
RubSmalltalkEditor >> selectionForDoitAsStream [
	"Answer a ReadStream on the text in the paragraph that is currently
	selected. "
	^ ReadWriteStream
		on: self string
		from: self startIndex 
		to: self stopIndex - 1
]

{ #category : #accessing }
RubSmalltalkEditor >> selectionPosition: aString [

	| startIndex |
	startIndex := self startIndex.
	^[
		| bottomLeft topLeft index |
		index := startIndex - aString size.
		self selectInvisiblyFrom: index to: index - 1.
		bottomLeft := textArea cursor bottomLeft.
		topLeft := textArea owner positionInWorld.
		topLeft + bottomLeft - textArea offset]
			ensure: [ self selectInvisiblyFrom: startIndex to: startIndex - 1 ]
]

{ #category : #'do-its' }
RubSmalltalkEditor >> selectionString [
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

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooseQuerySymbol [
	| pref |
	self hasSelection
		ifFalse: [ self selectPrecedingIdentifier ].
	pref := String
		streamContents: [ :strm | 
			self selection asString trimBoth
				do: [ :c | 
					c isSeparator
						ifFalse: [ strm nextPut: c ] ] ].
	selectorChooser := RubSelectorChooserMorph popUpPrefix: pref from: textArea
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooser [
	^ selectorChooser

]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserChosenSelector: aSelector [
	| newStart replacement caret prevSelection |
	self selectorChooserClose.
	aSelector ifNil: [ ^ self ].
	self selectPrecedingIdentifier.
	prevSelection := textArea selection.
	newStart := self startIndex.
	caret := self stopIndex.
	replacement := String
		streamContents: [ :strm | 
			| kwds |
			1 to: (kwds := aSelector keywords) size do: [ :i | 
				strm nextPutAll: (kwds at: i).
				i = 1
					ifTrue: [ caret := newStart + strm contents size + 1 ].
				strm
					nextPutAll:
						(i < kwds size
							ifTrue: [ '  ' ]
							ifFalse: [ ' ' ]) ] ].
	prevSelection asString = replacement trimBoth
		ifTrue: [ textArea flash ]
		ifFalse: [ 
			self replaceSelectionWith: replacement.
			self selectFrom: caret to: caret - 1 ]
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserClose [
	selectorChooser 
		ifNotNil: [ [selectorChooser delete] ensure: [selectorChooser := nil] ]
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserFocusChange [
	textArea hasFocus
		ifTrue: [ selectorChooser ifNotNil: [ selectorChooser comeToFront ] ]
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserHandlesKeyboard: aKeyboardEvent [
	"Return true or false on whether the code editor can handle keyboard event"
	^ (selectorChooser notNil and: [ selectorChooser handlesKeyboard: aKeyboardEvent ])

]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserKeystroke: aKeyboardEvent [
	| asciiValue |
	selectorChooser 
		ifNotNil: [(selectorChooser handlesKeyStroke: aKeyboardEvent) 
			ifTrue: [selectorChooser keyStroke: aKeyboardEvent.
				^ true]].
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue
		ifTrue: [self selectorChooserClose.
			^ true].
	aKeyboardEvent controlKeyPressed
		ifTrue: [self selectorChooserClose.
			^ true].
	"aKeyboardEvent commandKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true]."
	self selectorChooserUpdate.
	^ false
]

{ #category : #'basic-completion' }
RubSmalltalkEditor >> selectorChooserUpdate [
	selectorChooser ifNil: [^ self].
	self selectorChooserClose.
	self selectorChooseQuerySymbol 
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> sendersOf: selectedSelector [
	" opens a Browser on the given selector"
	self model systemNavigation browseAllSendersOf: selectedSelector
]

{ #category : #'menu messages' }
RubSmalltalkEditor >> sendersOfIt [
	"Open a senders browser on the selected selector"

	| selectedSelector syst |
	"self lineSelectAndEmptyCheck: [ ^ self ]."
	selectedSelector := self selectedSelector.
	selectedSelector isNil
		ifTrue: [ ^ textArea flash ].
	syst := self model interactionModel systemNavigation.
	syst browseAllSendersOrUsersOf: selectedSelector
]

{ #category : #'editing keys' }
RubSmalltalkEditor >> sendersOfIt: aKeyboardEvent [
	"Triggered by Cmd-n; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"

	self sendersOfIt.
	^ true
]

{ #category : #'do-its' }
RubSmalltalkEditor >> tally: aStream [
	"Treat the current selection as an expression; evaluate and tally it."
	| receiver context v compiledMethod profiler |
	(self model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := self model doItReceiver.
			context := self model doItContext]
		ifFalse:
			[receiver := context := nil].
	compiledMethod := self compile: aStream for: receiver in: context.
	compiledMethod ifNil: [^ self].
	profiler := TimeProfiler spyOn: [ v:= compiledMethod valueWithReceiver: receiver arguments: #()].
	profiler showResult: v.
	

]

{ #category : #'do-its' }
RubSmalltalkEditor >> tallyHighlight [
	"Treat the current highlight as an expression; evaluate and tally it."
	
	self highlightAndEmptyCheck: [ ^ self ].
	self tally: self highlightedTextAsStream
	

]

{ #category : #'do-its' }
RubSmalltalkEditor >> tallyIt [

	^ self tallySelection
]

{ #category : #'do-its' }
RubSmalltalkEditor >> tallySelection [
	"Treat the current selection as an expression; evaluate and tally it."

	self lineSelectAndEmptyCheck: [ ^ self ].
	self lineSelectAndEmptyCheck: [ ^ self ].
	self tally: self selectionAsStream
	

]
