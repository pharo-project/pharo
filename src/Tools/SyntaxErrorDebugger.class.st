"
I am a special Debugger for syntax errors encountered when filing in class descriptions from a non-interactive source such as an external file. As a StringHolder, the string to be viewed is the method code or expression containing the error.

The user may fix the error and accept the method to continue the fileIn.

"
Class {
	#name : #SyntaxErrorDebugger,
	#superclass : #Object,
	#instVars : [
		'contents',
		'class',
		'selector',
		'category',
		'debugSession',
		'doitFlag',
		'dependents',
		'location',
		'error',
		'announcer',
		'syntaxError'
	],
	#category : #'Tools-Debugger'
}

{ #category : #'instance creation' }
SyntaxErrorDebugger class >> buildMorphicViewOn: aSyntaxError [
	"Answer an Morphic view on the given SyntaxError."

	| window |
	window := (SystemWindow labelled: 'Syntax Error: ' , aSyntaxError error)
		model: aSyntaxError.
	window
		onAnnouncement: WindowClosed do: [ 
			aSyntaxError handleWindowClosed ];
		addMorph:
			((PluggableListMorph
				on: aSyntaxError
				list: #list
				selected: #listIndex
				changeSelected: nil
				menu: #listMenu:) doubleClickSelector: #listDoubleClick)
		frame: (0 @ 0 corner: 1 @ 0.15).
	window
		addMorph:
			(RubScrolledTextMorph new
				on: aSyntaxError
				text: #contents
				accept: #contents:notifying:
				readSelection: #contentsSelection
				menu: #codePaneMenu:shifted:)
		frame: (0 @ 0.15 corner: 1 @ 1).
	aSyntaxError addDependant: window.
	^ window openInWorldExtent: 380 @ 220
]

{ #category : #'instance creation' }
SyntaxErrorDebugger class >> open: aSyntaxError [ 
	"Answer a standard system view whose model is an instance of me."
	<primitive: 19>
	"Simulation guard"
	| process |
	process := Processor activeProcess.
	UIManager default spawnNewProcessIfThisIsUI: process.
	UIManager default defer: [
		self buildMorphicViewOn: aSyntaxError.
	].
	^ process suspend
]

{ #category : #'tools registry' }
SyntaxErrorDebugger class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #syntaxErrorDebugger

]

{ #category : #'instance creation' }
SyntaxErrorDebugger class >> syntaxError: aSyntaxError [
	
	^ self new
		syntaxError: aSyntaxError;
		yourself
		
]

{ #category : #'dependents access' }
SyntaxErrorDebugger >> addDependant: aDependant [
	dependents add: aDependant
]

{ #category : #accessing }
SyntaxErrorDebugger >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #menu }
SyntaxErrorDebugger >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	| myClass |
	(myClass := self selectedClassOrMetaClass) ifNotNil:
		[Smalltalk tools browser
			openOnClass: myClass selector: self selectedMessageName]
]

{ #category : #accessing }
SyntaxErrorDebugger >> category: aSymbol [
	"Record the message category of method being compiled. This is used when the user corrects the error and accepts."

	category := aSymbol.

]

{ #category : #initialization }
SyntaxErrorDebugger >> checkForUnprintableCharacters: aString [
	| types printables |
	types := String typeTable.
	printables := '!@#$%&*-_=+<>{}?/\,¬∑¬£¬¢¬ß¬∂¬™¬∫¬ñ¬ó¬ì¬ë¬î¬í¬Ö√ö√¶√ö¬Ø√ó¬ø¬´¬ª`~`' asSet.
	^ aString
		detect: [ :aChar | (types at: aChar asciiValue ifAbsent: [ #xLetter ]) == #xBinary and: [ (printables includes: aChar) not ] ]
		ifFound: [ :badChar | 
			'<<<This string contains a character (ascii value ' , badChar asciiValue printString
				, ') that is not normally used in code>>> ' , aString ]
		ifNone: [ aString ]
]

{ #category : #other }
SyntaxErrorDebugger >> closeWindow [

	self topView close

]

{ #category : #accessing }
SyntaxErrorDebugger >> codePaneMenu: aMenu shifted: shifted [
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"

	| donorMenu |
	donorMenu := (PragmaMenuBuilder pragmaKeyword: RubSmalltalkCodeMode menuKeyword model: self)
		menu.
	^ aMenu addAllFrom: donorMenu
]

{ #category : #accessing }
SyntaxErrorDebugger >> contents [
	^contents 
]

{ #category : #accessing }
SyntaxErrorDebugger >> contents: stringOrText [
	contents := stringOrText
]

{ #category : #other }
SyntaxErrorDebugger >> contents: aString notifying: aController [
	"Compile the code in aString and proceed. Do not notify anybody of errors, because nobody would have been notified of errors if this syntax error had not arisen"

	aController hasUnacceptedEdits: false.
	
	doitFlag
		ifTrue: [ self class compiler evaluate: aString ]
		ifFalse: [ self resume: aString ].
	self closeWindow
]

{ #category : #other }
SyntaxErrorDebugger >> contentsSelection [
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^ location 
		ifNil: [1 to: 0]  "null selection"
		ifNotNil: [ location to: location + self sourceErrorString size - 1 ] 
]

{ #category : #menu }
SyntaxErrorDebugger >> debug [
	"Show the stack of the process leading to this syntax editor, typically showing the stack of the compiler as called from fileIn."

	^ Smalltalk tools debugger openOn: debugSession withFullView: true 
]

{ #category : #accessing }
SyntaxErrorDebugger >> error [
	^ error
]

{ #category : #menu }
SyntaxErrorDebugger >> handleWindowClosed [
	debugSession interruptedProcess == nil 
		ifFalse: [ 
			debugSession 
			resume: nil;
			clear ]
]

{ #category : #initialization }
SyntaxErrorDebugger >> highlightError [
	"copy in the error message"
	contents := contents
					copyReplaceFrom: location
					to: location - 1
					with: self sourceErrorString.
	"convert to Text that we can add Attributes"
	contents := contents asText.
	"use the ST80Styler"
	location ifNil: [ ^ self ].	
	contents addAttribute: TextColor red from: location to: location + self sourceErrorString size - 1.
	contents addAttribute: TextEmphasis bold from: location to: location + self sourceErrorString size - 1.
]

{ #category : #initialization }
SyntaxErrorDebugger >> initialize [
	dependents := Set new.
]

{ #category : #'message list' }
SyntaxErrorDebugger >> list [
	"Answer an array of one element made up of the class name, message category, and message selector in which the syntax error was found. This is the single item in the message list of a view/browser on the receiver."

	selector ifNil: [^ Array with: (class name, '  ', (category ifNil: ['']), '  ', '<none>')].
	category ifNil: [^ Array with: (class name, '    ', '<none>')].
	^ Array with: (class name, '  ', category, '  ', selector)

]

{ #category : #'message list' }
SyntaxErrorDebugger >> listDoubleClick [
	self debug
]

{ #category : #'message list' }
SyntaxErrorDebugger >> listIndex [
	"There is always exactly one element in my list and it is always selected."

	^ 1

]

{ #category : #menu }
SyntaxErrorDebugger >> listMenu: aMenu [
	^aMenu addList: {
		{'Proceed' . #proceed}.
		{'Debug calling process' . #debug}.
		{'Browse full translated' . #browseMethodFull}}.
]

{ #category : #accessing }
SyntaxErrorDebugger >> model [
	^ self
]

{ #category : #other }
SyntaxErrorDebugger >> notify: anError at: aLocation in: source [
	"Open a syntax error view, inserting the given error message into the given source at the given location. This message is sent to the 'requestor' when the parser or compiler finds a syntax error."

	| aClass  |
	aClass := thisContext sender receiver encoder classEncoding.
	self setClass: aClass
		code: source
		error: anError
		location: aLocation
		debugSession: (Processor activeProcess newDebugSessionNamed: 'Stack of the Syntax Error' startedAt: thisContext)
		doitFlag: false.
	self class open: self.

]

{ #category : #menu }
SyntaxErrorDebugger >> proceed [
	"The user has has edited and presumably fixed the syntax error and the filein can now proceed."

	debugSession 
			resume;
			clear
]

{ #category : #initialization }
SyntaxErrorDebugger >> release [
	"Prevent that syntaxError window lets a suspended compiler process running, when the window is closed."

	debugSession ifNotNil: [ debugSession terminate ].
	self releaseActionMap. "we are not sure if we need it"
	super release.
]

{ #category : #menu }
SyntaxErrorDebugger >> resume: aValue [

	debugSession 
			resume: aValue;
			clear
]

{ #category : #'text menu support' }
SyntaxErrorDebugger >> selectedClass [
	"Answer the class in which the syntax error occurred."

	^ class

]

{ #category : #'text menu support' }
SyntaxErrorDebugger >> selectedClassOrMetaClass [
	"Answer the class of the method being compiled."

	^ class

]

{ #category : #'text menu support' }
SyntaxErrorDebugger >> selectedMessageName [
	"Answer the selector of the method being compiled."

	^ selector

]

{ #category : #initialization }
SyntaxErrorDebugger >> setClass: aClass code: aString error: errorMessage location: anErrorPosition debugSession: aDebugSession doitFlag: flag [

	class := aClass.
	error := errorMessage.
	location := anErrorPosition.	
	debugSession := aDebugSession.
	selector := aClass compiler parseSelector: aString.
	contents := self checkForUnprintableCharacters: aString.
	self highlightError.
		
	category ifNil: [ category := aClass organization categoryOfElement: selector ].
	category ifNil: [ category := Protocol unclassified ].
	doitFlag := flag
]

{ #category : #other }
SyntaxErrorDebugger >> shoutAboutToStyle: aPluggableShoutMorphOrView [
	^ true
]

{ #category : #accessing }
SyntaxErrorDebugger >> sourceErrorString [
	^ error , ' -> '
]

{ #category : #initialization }
SyntaxErrorDebugger >> syntaxError: aSyntaxError [
	"extract the instance properties from a SyntaxErrorNotification"
	
	syntaxError := aSyntaxError.
	self 
		setClass: aSyntaxError errorClass
		code: aSyntaxError errorCode
		error: aSyntaxError errorMessage
		location: aSyntaxError location
		debugSession: (Processor activeProcess newDebugSessionNamed: 'Stack of the Syntax Error' startedAt: aSyntaxError signalerContext)
		doitFlag: aSyntaxError doitFlag.
]

{ #category : #accessing }
SyntaxErrorDebugger >> topView [
	"Find the first top view on me. Is there any danger of their being two
	with the same model? Any danger from ungarbage collected old views?
	Ask if schedulled?"
	dependents
		ifNil: [^ nil].
	dependents
		do: [:v | (v isSystemWindow
					and: [v isInWorld])
				ifTrue: [^ v]].
	^ nil
]
