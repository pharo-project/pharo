"
The Morphic ui manager. 

I was packaged in toolbuilder and I may change in the future. For the moment, the goal is to unload toolbuilder. 
"
Class {
	#name : 'MorphicUIManager',
	#superclass : 'UIManager',
	#traits : 'TEasilyThemed - {#theme}',
	#classTraits : 'TEasilyThemed classTrait',
	#instVars : [
		'activeTranscript'
	],
	#classVars : [
		'UIProcess'
	],
	#category : 'Polymorph-Widgets',
	#package : 'Polymorph-Widgets'
}

{ #category : 'accessing' }
MorphicUIManager class >> isActiveManager [
	"Answer whether I should act as the active ui manager"
	^ true
]

{ #category : 'testing' }
MorphicUIManager class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isHeadless not
		or: [
			Smalltalk isHeadless
			and: [ Smalltalk isInteractiveGraphic ] ]
]

{ #category : 'cleanup' }
MorphicUIManager class >> restartMethods [
   UIProcess ifNotNil: [
	| process |
	process := UIProcess.
	self new spawnNewProcess.
	process terminate. ]
]

{ #category : 'ui process' }
MorphicUIManager class >> uiProcess [

	^ UIProcess
]

{ #category : 'private' }
MorphicUIManager >> activate [

	self currentWorld worldState worldRenderer: (AbstractWorldRenderer detectCorrectOneForWorld: self currentWorld).
	self currentWorld currentCursor: self currentWorld currentCursor.

	self currentWorld ifNotNil: [:world | world restoreMorphicDisplay].
	WorldMorph extraWorldList do: [:each | each restoreMorphicDisplay].

	activeTranscript
		ifNotNil: [ 
			Transcript := activeTranscript ]
		ifNil: [
			Transcript class == ThreadSafeTranscript 
				ifFalse: [ ThreadSafeTranscript install ] ].
			
	SystemProgressMorph enable
]

{ #category : 'ui requests' }
MorphicUIManager >> chooseDirectory: label from: dir [
	"Answer the user choice of a directory."

	| modalMorph realLabel |
	realLabel := label ifNil: ['Choose Directory' translated].
	(ProvideAnswerNotification signal: realLabel) ifNotNil: [:answer |
		^answer ].
	modalMorph := self modalMorph.
	^modalMorph theme
		chooseDirectoryIn: modalMorph
		title: realLabel
		path: (dir ifNotNil: [dir fullName])
]

{ #category : 'services' }
MorphicUIManager >> chooseFrom: aList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."

	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		message: messageString
		title: aString) ifNil: [0]
]

{ #category : 'ui requests' }
MorphicUIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."

	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		title: aString) ifNil: [0]
]

{ #category : 'services' }
MorphicUIManager >> chooseFrom: labelList values: valueList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."

	^ self theme
		chooseIn: self modalMorph
		title: aString
		message: messageString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : 'ui requests' }
MorphicUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."

	|modalMorph|

	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer |
		^answer].

	modalMorph := self modalMorph.
	^modalMorph theme
		chooseIn: modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : 'ui requests' }
MorphicUIManager >> chooseFullFileNameMatching: patterns label: label [
	"Let the user choose a file matching the given patterns"

	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme
		chooseFullFileNameIn: modalMorph
		title: (label ifNil: ['Choose File' translated])
		patterns: patterns
		path: nil
		preview: false
]

{ #category : 'services' }
MorphicUIManager >> chooseOrRequestFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list.
	 Answer the value selected of the selected item or the new string entered."

	^self
		chooseOrRequestFrom: aList
		values: aList
		lines: linesArray
		title: aString
]

{ #category : 'ui requests' }
MorphicUIManager >> chooseOrRequestFrom: aList title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseOrRequestFrom: aList lines: #() title: aString
]

{ #category : 'services' }
MorphicUIManager >> chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list.
	 Answer the value selected of the selected item or the new string entered."

	^ self theme
		chooseOrRequestIn: self modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : 'ui requests' }
MorphicUIManager >> confirm: aStringOrText [
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."

	^ self confirm: aStringOrText label: 'Question' translated
]

{ #category : 'ui requests' }
MorphicUIManager >> confirm: questionStringOrText label: labelStringOrText [
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."

	|modalMorph|
	(ProvideAnswerNotification signal: questionStringOrText) ifNotNil: [:answer |
		^answer].
	modalMorph := self modalMorph.
	^modalMorph theme
		questionWithoutCancelIn: modalMorph
		text: questionStringOrText
		title: labelStringOrText
]

{ #category : 'services' }
MorphicUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."

	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^answer].
	^ self theme
		customQuestionIn: self modalMorph
		text: queryString
		yesText: trueChoice
		noText: falseChoice
		cancelText: cancelChoice
		default: defaultOption
		title: title
]

{ #category : 'ui requests' }
MorphicUIManager >> confirm: aStringOrText orCancel: cancelBlock [
	"Put up a question dialog (with cancel) with the text queryString.
	Answer true if the response is yes, false if no.
	Answer the value of the cancel block if cancelled.
	This is a modal question--the user must respond yes or no or cancel."

	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #cancel ifTrue: [cancelBlock value] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme
		questionIn: modalMorph
		text: aStringOrText
		title: 'Question' translated) ifNil: cancelBlock
]

{ #category : 'services' }
MorphicUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice, false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."

	^self
		confirm: queryString
		trueChoice: trueChoice
		falseChoice: falseChoice
		cancelChoice: nil
		default: nil
]

{ #category : 'services' }
MorphicUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."

	^ self
		confirm: queryString
		label: 'Question' translated
		trueChoice: trueChoice
		falseChoice: falseChoice
		cancelChoice: cancelChoice
		default: defaultOption
]

{ #category : 'private' }
MorphicUIManager >> deactivate [

	activeTranscript := Transcript.
	SystemProgressMorph disable.

	self triggerEvent: #aboutToLeaveWorld.
	WorldMorph extraWorldList do: [:world | world triggerEvent: #aboutToLeaveWorld ].

	self currentWorld worldState worldRenderer: (NullWorldRenderer forWorld: self currentWorld)
]

{ #category : 'debug' }
MorphicUIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [

	(OupsDebugRequest newForContext: context)
		process: process;
		label: title;
		submit
]

{ #category : 'ui process' }
MorphicUIManager >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self currentWorld defer: aValuable
]

{ #category : 'ui requests' }
MorphicUIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"

	| window pane |
	window := (SystemWindow labelled: labelString) model: self.
	pane := RubScrolledTextMorph new
				setText: aText asString;
				yourself.
	aBlockOrNil ifNotNil: [
		pane announcer when: RubTextAcceptRequest do: [:aRequest | aBlockOrNil value: aRequest morph text] for: self].
	window addMorph: pane frame: (0@0 corner: 1@1); openInWorld.
	^ window
]

{ #category : 'services' }
MorphicUIManager >> enterOrRequestFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list.
	 Answer the value selected of the selected item or the new string entered."

	^self
		enterOrRequestFrom: aList
		values: aList
		lines: linesArray
		title: aString
]

{ #category : 'ui requests' }
MorphicUIManager >> enterOrRequestFrom: aList title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self enterOrRequestFrom: aList lines: #() title: aString
]

{ #category : 'services' }
MorphicUIManager >> enterOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list.
	 Answer the value selected of the selected item or the new string entered."

	^ self theme
		enterOrRequestIn: self modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : 'handle debug requests' }
MorphicUIManager >> handleDebugRequest: aDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [
	<debuggerCompleteToSender>
	anOupsDebuggerSystem openDebuggerOnRequest: aDebugRequest
]

{ #category : 'error handling' }
MorphicUIManager >> handleError: anError log: shouldLog [

	shouldLog ifTrue: [ self logError: anError ].
	(OupsDebugRequest newForException: anError) submit
]

{ #category : 'error handling' }
MorphicUIManager >> handleNotification: aNotification [

	aNotification messageText traceCr.
	aNotification resume
]

{ #category : 'error handling' }
MorphicUIManager >> handleWarning: aWarning [
	"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."

	(OupsWarningRequest newForException: aWarning) submit
]

{ #category : 'handle debug requests' }
MorphicUIManager >> handleWarningDebugRequest: aWarningDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [
	<debuggerCompleteToSender>
	anOupsDebuggerSystem openDebuggerOnRequest: aWarningDebugRequest
]

{ #category : 'ui requests' }
MorphicUIManager >> inform: aStringOrText [
	"Display a message for the user to read and then dismiss."

	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^true].
	GrowlMorph
		openWithLabel: 'Information' translated
		contents: aStringOrText
]

{ #category : 'ui requests' }
MorphicUIManager >> informUserDuring: aBlock [
	"Display a message as progress	during execution of the given block."
	"UIManager default informUserDuring: [:bar|
		#('one' 'two' 'three') do: [:info|
			bar label: info.
			1 to: 100 do: [:v |
				bar current: v.
				(Delay forMilliseconds: 20) wait]]]"

	self
		displayProgress: ''
		from: 1 to: 100
		during: [:bar | aBlock value: bar]
]

{ #category : 'debug' }
MorphicUIManager >> lowSpaceWarningMessage [
	"Return a notifier message string to be presented when space is running low."

	^ 'Warning! Pharo is almost out of memory!

Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don''t panic, but do proceed with caution.

Here are some suggestions:

 If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.

 If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
   > Close any windows that are not needed.
   > Get rid of some large objects (e.g., images).
   > Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Pharo VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.

 If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!).

'
]

{ #category : 'accessing' }
MorphicUIManager >> modalMorph [
	"Answer the morph that should be used to handle modality."
	| sender receiver foundWorld |
	sender := thisContext sender.
	foundWorld := false.
	[foundWorld or: [sender isNil]]
		whileFalse: [receiver := sender receiver.
			((sender selector = #invokeWorldMenu:)
					or: [receiver == self currentWorld
							and: [sender selector == #handleEvent:
									or: [sender selector == #findWindow:]]])
				ifTrue: [foundWorld := true]
				ifFalse: [sender := sender sender]].
	foundWorld
		ifTrue: [^ receiver world
				ifNil: [self currentWorld]].
	^ SystemWindow topWindow
		ifNil: [self currentWorld]
]

{ #category : 'ui requests' }
MorphicUIManager >> multiLineRequest: queryString  initialAnswer: defaultAnswer answerHeight: answerHeight [
	"Create a multi-line instance of me whose question is queryString with
	the given initial answer. Answer the string the user accepts.  Answer nil if the user cancels.  An
	empty string returned means that the ussr cleared the editing area and
	then hit 'accept'.  Because multiple lines are invited, we ask that the user
	use the ENTER key, or (in morphic anyway) hit the 'accept' button, to
	submit; that way, the return key can be typed to move to the next line."

	|modalMorph|
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEditorIn: modalMorph
		text: queryString
		title: 'Information Required' translated
		entryText: defaultAnswer
		entryHeight: answerHeight)
			ifNotNil: [:text | text asString]
]

{ #category : 'ui requests' }
MorphicUIManager >> newMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."
	"UIManager default"
	^self theme
		newMenuIn: aThemedMorph for: aModel
]

{ #category : 'ui requests' }
MorphicUIManager >> onFontsChanged [

	StringMorph allSubInstancesDo: [ :s | s layoutChanged ].
	TextMorph allSubInstancesDo: [ :s | s layoutChanged ].
	SystemWindow allInstancesDo: [ :w | [ w update: #relabel ] on: Error do: [ :ex | ] ].
	self currentWorld ifNotNil: [ :w | w changed ]
]

{ #category : 'events' }
MorphicUIManager >> onPrimitiveError: aString [

	| message |
	message := String streamContents: [ :s |
		           | context |
		           s nextPutAll: '*** System error handling failed ***'.
		           s
			           cr;
			           nextPutAll: aString.
		           context := thisContext sender sender.
		           20 timesRepeat: [
			           context ifNotNil: [
				           s
					           cr;
					           print: (context := context sender) ] ] ].
	Stdio stderr << message.
	"init hands and redisplay"
	self currentWorld install
]

{ #category : 'accessing' }
MorphicUIManager >> preferredCornerStyle [
	^ self theme preferredCornerStyle
]

{ #category : 'ui requests' }
MorphicUIManager >> request: queryString initialAnswer: defaultAnswer [
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."

	^ self
		  request: queryString
		  initialAnswer: defaultAnswer
		  title: 'Provide the following information'
]

{ #category : 'ui requests' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	^ self request: aStringOrText initialAnswer: defaultAnswer title: 'Information Required' translated entryCompletion: anEntryCompletion
]

{ #category : 'ui requests' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle [
	"Create an instance of me whose question is queryString with the given
	initial answer. Answer the string the user accepts.
	The title serves for the window that is opened
	Answer the empty string if the user cancels.
	Allow for interception with a ProvideAnswerNotification handler."

	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEntryIn: modalMorph
		text: aStringOrText
		title: aTitle
		entryText: defaultAnswer)
]

{ #category : 'ui requests' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [
	"Create an instance of me whose question is queryString with the given
	initial answer. Answer the string the user accepts.
	The title serves for the window that is opened
	Answer the empty string if the user cancels.
	Allow for interception with a ProvideAnswerNotification handler."

	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEntryIn: modalMorph
		text: aStringOrText
		title: aTitle
		entryText: defaultAnswer
		entryCompletion: anEntryCompletion)
]

{ #category : 'ui requests' }
MorphicUIManager >> requestPassword: aStringOrText [
	"Request for a password.
	Allow for interception with a ProvideAnswerNotification handler.
	Answer nil if the user cancels."

	|modalMorph|
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^answer == #default ifTrue: [''] ifFalse: [answer]].
	modalMorph := self modalMorph.
	^modalMorph theme
		passwordEntryIn: modalMorph
		text: aStringOrText
		title: 'Password Required' translated
		entryText: ''
]

{ #category : 'ui process' }
MorphicUIManager >> resumeUIProcess: aProcess [
	"Adopt aProcess as the project process -- probably because of proceeding from a debugger"

	UIProcess := aProcess.
	UIProcess resume
]

{ #category : 'display' }
MorphicUIManager >> showWaitCursorWhile: aBlock [
	Cursor wait showWhile: aBlock
]

{ #category : 'ui process' }
MorphicUIManager >> spawnNewProcess [

	UIProcess := [ MorphicRenderLoop new doOneCycleWhile: [ true ] ] newProcess priority:
		             Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI Process';
		resume
]

{ #category : 'ui process' }
MorphicUIManager >> terminateUIProcess [
	UIProcess suspend; terminate.
	UIProcess := nil "?"
]

{ #category : 'accessing' }
MorphicUIManager >> theme [
	^ Smalltalk ui theme
]

{ #category : 'ui process' }
MorphicUIManager >> uiProcess [
	" Answer the currently active UI process for morphic world.
	Client should check explicitly if #uiProcess answers nil or not (see other implementations)"

	^ UIProcess
]

{ #category : 'ui requests' }
MorphicUIManager >> unhandledErrorDefaultAction: anException [

	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."
	anException debug
]

{ #category : 'global state' }
MorphicUIManager >> world [
	^ self currentWorld
]
