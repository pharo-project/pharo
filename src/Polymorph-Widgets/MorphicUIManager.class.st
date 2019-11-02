"
The Morphic ui manager. 

I was packaged in toolbuilder and I may change in the future. For the moment, the goal is to unload toolbuilder. 
"
Class {
	#name : #MorphicUIManager,
	#superclass : #UIManager,
	#traits : 'TEasilyThemed - {#theme}',
	#classTraits : 'TEasilyThemed classTrait',
	#instVars : [
		'activeTranscript'
	],
	#classVars : [
		'UIProcess'
	],
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
MorphicUIManager class >> isActiveManager [
	"Answer whether I should act as the active ui manager"
	^ true
]

{ #category : #testing }
MorphicUIManager class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isHeadless not or: [ Smalltalk isHeadless and: [ CommandLineArguments new hasOption: 'interactive'  ] ]
]

{ #category : #private }
MorphicUIManager >> activate [

	World worldState worldRenderer: (AbstractWorldRenderer detectCorrectOneforWorld: World).
	Cursor currentCursor: Cursor currentCursor.
		
	WorldMorph currentWorld ifNotNil: [:world | world restoreMorphicDisplay].
	WorldMorph extraWorldList do: #restoreMorphicDisplay.

	activeTranscript 
		ifNil: [
			Transcript class == ThreadSafeTranscript ifFalse: [ 
				ThreadSafeTranscript install 
			]
		]		
		ifNotNil: [ Transcript := activeTranscript ].
		
	SystemProgressMorph enable.
]

{ #category : #'ui requests' }
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

{ #category : #services }
MorphicUIManager >> chooseFrom: aList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."
	
	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		message: messageString
		title: aString) ifNil: [0]
]

{ #category : #'ui requests' }
MorphicUIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		title: aString) ifNil: [0]
]

{ #category : #services }
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

{ #category : #'ui requests' }
MorphicUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."
	
	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme  
		chooseIn: modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : #services }
MorphicUIManager >> chooseFullFileName: title extensions: exts path: path preview: preview [
	"Answer the result of a file name chooser dialog with the given title, extensions
	to show, path and preview type."

	^self theme
		chooseFullFileNameIn: self modalMorph
		title: title
		extensions: exts
		path: path
		preview: preview
]

{ #category : #'ui requests' }
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

{ #category : #services }
MorphicUIManager >> chooseOrRequestFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. 
	 Answer the value selected of the selected item or the new string entered."
	
	^self
		chooseOrRequestFrom: aList
		values: aList
		lines: linesArray
		title: aString
]

{ #category : #'ui requests' }
MorphicUIManager >> chooseOrRequestFrom: aList title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseOrRequestFrom: aList lines: #() title: aString
]

{ #category : #services }
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

{ #category : #'ui requests' }
MorphicUIManager >> confirm: aStringOrText [
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."
	
	^ self confirm: aStringOrText label: 'Question' translated.
]

{ #category : #'ui requests' }
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
		title: labelStringOrText.
]

{ #category : #services }
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

{ #category : #'ui requests' }
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

{ #category : #services }
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

{ #category : #services }
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

{ #category : #'ui requests' }
MorphicUIManager >> currentWorld [

	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : #private }
MorphicUIManager >> deactivate [
	activeTranscript := Transcript.
	SystemProgressMorph disable.
	
	self triggerEvent: #aboutToLeaveWorld.
	WorldMorph extraWorldList do: [:world | world triggerEvent: #aboutToLeaveWorld ].	
	
	World worldState worldRenderer: (NullWorldRenderer forWorld: World).
]

{ #category : #debug }
MorphicUIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [
	"Open a notifier in response to an error, halt, or notify. A notifier view
	just shows a short view of the sender stack and provides a menu that
	lets the user open a full debugger."

	| debugSession |

	debugSession := process newDebugSessionNamed: title startedAt: context.
	
	debugSession logStackToFileIfNeeded.
	debugSession isAboutUIProcess ifTrue: [ 
		DefaultExecutionEnvironment beActiveDuring: [self spawnNewProcess]].
		
	self defer:	[ 
			"schedule debugger in deferred UI message to address
			redraw problems after opening a debugger e.g. from
			the testrunner."
		[ Smalltalk tools debugger 
				openOn: debugSession withFullView: bool andNotification: notificationString.
		] on: Error do: [ :ex | 	debugSession signalDebuggerError: ex] 
	].

	process suspend.

]

{ #category : #'ui process' }
MorphicUIManager >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	WorldState defer: aValuable
]

{ #category : #'ui requests' }
MorphicUIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"
	
	| window pane |
	window := (SystemWindow labelled: labelString) model: self.
	pane := RubScrolledTextMorph new 
				setText: aText asString;
				yourself.
	aBlockOrNil ifNotNil: [  
		pane announcer when: RubTextAcceptRequest do: [:aRequest | aBlockOrNil value: aRequest morph text]].	
	window addMorph: pane frame: (0@0 corner: 1@1); openInWorld.
	^ window
		
]

{ #category : #services }
MorphicUIManager >> enterOrRequestFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. 
	 Answer the value selected of the selected item or the new string entered."
	
	^self
		enterOrRequestFrom: aList
		values: aList
		lines: linesArray
		title: aString
]

{ #category : #'ui requests' }
MorphicUIManager >> enterOrRequestFrom: aList title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self enterOrRequestFrom: aList lines: #() title: aString
]

{ #category : #services }
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

{ #category : #'ui requests' }
MorphicUIManager >> fontFromUser: priorFont [	
	
	^ StrikeFont fromUser: priorFont allowKeyboard: true
]

{ #category : #'error handling' }
MorphicUIManager >> handleError: anError log: shouldLog [

	shouldLog ifTrue: [ self logError: anError ].
	self debugProcess: Processor activeProcess  
		context: anError signalerContext
		label: anError description 
		fullView: false
]

{ #category : #'ui requests' }
MorphicUIManager >> inform: aStringOrText [
	"Display a message for the user to read and then dismiss."
	
	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^true].
	GrowlMorph 
		openWithLabel: 'Information' translated 
		contents: aStringOrText
]

{ #category : #'ui requests' }
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

{ #category : #debug }
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

{ #category : #debug }
MorphicUIManager >> lowSpaceWatcherDefaultAction: theInterruptedProcess [
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess projectProcess debugSession |
	ActiveHand ifNotNil:[ActiveHand interrupted].
	ActiveWorld := self currentWorld. "reinstall active globals"
	ActiveHand := self currentWorld primaryHand.
	ActiveHand interrupted. "make sure this one's interrupted too"
	ActiveEvent := nil.

	projectProcess := self uiProcess.	"we still need the accessor for a while"
	preemptedProcess := theInterruptedProcess ifNil: [Processor preemptedProcess].
	"Only debug preempted process if its priority is >= projectProcess' priority"
	preemptedProcess priority < projectProcess priority 
		ifTrue:[preemptedProcess := projectProcess].
	preemptedProcess suspend.
	
	debugSession := preemptedProcess 
		newDebugSessionNamed: 'Space is low' 
		startedAt: preemptedProcess suspendedContext.		
	debugSession logStackToFileIfNeeded.
	debugSession isAboutUIProcess ifTrue: [ 
		DefaultExecutionEnvironment beActiveDuring: [self spawnNewProcess]].
	
	Smalltalk tools debugger 
		openOn: debugSession 
		withFullView: false 
		andNotification: self lowSpaceWarningMessage
]

{ #category : #'ui requests' }
MorphicUIManager >> merge: merger informing: aString [

	| mergeMorph  window |
	mergeMorph := PSMCMergeMorph forMerger: merger.
	mergeMorph
		fromDescription: 'Working copy' translated;
		toDescription: aString.
	window := mergeMorph newWindow
		title: aString;
		yourself.
	self modalMorph openModal: window.
	^ mergeMorph merged
]

{ #category : #accessing }
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

{ #category : #'ui requests' }
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

{ #category : #'ui requests' }
MorphicUIManager >> newDisplayDepthNoRestore: pixelSize [

	"Change depths.  Check if there is enough space!  , di"
	| area need |
	pixelSize = Display depth ifTrue: [^ Display  "no change"].
	pixelSize abs < Display depth ifFalse:
		["Make sure there is enough space"
		area := Display boundingBox area. "pixels"
		need := (area * (pixelSize abs - Display depth) // 8)  "new bytes needed"
				+ Smalltalk lowSpaceThreshold.
		(Smalltalk garbageCollectMost <= need
			and: [Smalltalk garbageCollect <= need])
			ifTrue: [self error: 'Insufficient free space']].
	Display setExtent: Display extent depth: pixelSize.

	DisplayScreen startUp
]

{ #category : #'ui requests' }
MorphicUIManager >> newMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."
	"UIManager default"
	^self theme 
		newMenuIn: aThemedMorph for: aModel
]

{ #category : #events }
MorphicUIManager >> onEventSensorStartup: anEventSensor [

	anEventSensor flushAllButDandDEvents
]

{ #category : #'ui requests' }
MorphicUIManager >> onFontsChanged [

	StringMorph allSubInstancesDo: [ :s | s layoutChanged ].
	TextMorph allSubInstancesDo: [ :s | s layoutChanged ].
	SystemWindow allInstancesDo: [ :w | [ w update: #relabel ] on: Error do: [ :ex | ] ].
	self currentWorld ifNotNil: [ :w | w changed ].
]

{ #category : #events }
MorphicUIManager >> onPrimitiveError: aString [ 
	| message |
	message := String
				streamContents: [:s | 
					| context |
					s nextPutAll: '*** System error handling failed ***'.
					s cr; nextPutAll: aString.
					context := thisContext sender sender.
					20
						timesRepeat: [context == nil
								ifFalse: [s cr; print: (context := context sender)]]].
	Transcripter askForEmergencyEvaluatorOrExitWithText: message.
	"init hands and redisplay" 
	self currentWorld install

]

{ #category : #'ui requests' }
MorphicUIManager >> openComparisonFrom: targetMethodSource
					to: originalMethodSource
					belongingTo: aClass
					from: aChange 
					labeled: aLabel 
					inWindowLabeled: aWindowLabel [ 
	| diffBuilder difference  |
	PSMCPatchMorph usedByDefault 
		ifTrue: [ ^ self openPolymorphComparisonFrom: originalMethodSource 
					to: targetMethodSource
					belongingTo: aClass
					from: aChange
					labeled: aLabel
					inWindowLabeled: aWindowLabel   ].
	diffBuilder :=  TextDiffBuilder
			from: originalMethodSource 
			to: targetMethodSource.
	difference := diffBuilder buildDisplayPatch.
	self edit: difference label: aLabel.
				
]

{ #category : #private }
MorphicUIManager >> openModal: aSystemWindow [ 
	"Open the given window at an available position with modality.
	Answer the system window."
	
	^self modalMorph openModal: aSystemWindow
]

{ #category : #private }
MorphicUIManager >> openPolymorphComparisonFrom: targetMethodSource
					to: originalMethodSource
					belongingTo: aClass
					from: aChange 
					labeled: aLabel 
					inWindowLabeled: aWindowLabel [ 
	| diffMorph |
	diffMorph := DiffChangeMorph
		from: targetMethodSource
		label: aChange stamp
		to: originalMethodSource
		label: (aClass compiledMethodAt: aChange methodSelector) timeStamp
		contextClass: aClass.
	diffMorph openInWindowLabeled: aWindowLabel.
]

{ #category : #accessing }
MorphicUIManager >> preferredCornerStyle [
	^ self theme preferredCornerStyle
]

{ #category : #'ui requests' }
MorphicUIManager >> request: queryString initialAnswer: defaultAnswer [
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."


	 ^ self 
		request: queryString 
		initialAnswer: defaultAnswer 
		title: 'Provide the following information'

]

{ #category : #'ui requests' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	^ self request: aStringOrText initialAnswer: defaultAnswer title: 'Information Required' translated entryCompletion: anEntryCompletion

]

{ #category : #'ui requests' }
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

{ #category : #'ui requests' }
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

{ #category : #'ui requests' }
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

{ #category : #'ui requests' }
MorphicUIManager >> restoreDisplay [

	self currentWorld fullRepaintNeeded
]

{ #category : #'ui requests' }
MorphicUIManager >> restoreDisplayAfter: aBlock [

	aBlock value.
	self currentWorld activeHand waitButton.
	self currentWorld fullRepaintNeeded.
]

{ #category : #'ui process' }
MorphicUIManager >> resumeUIProcess: aProcess [
	"Adopt aProcess as the project process -- probably because of proceeding from a debugger"
			
	UIProcess := aProcess.
	UIProcess resume
]

{ #category : #display }
MorphicUIManager >> showWaitCursorWhile: aBlock [
	Cursor wait showWhile: aBlock
]

{ #category : #'ui process' }
MorphicUIManager >> spawnNewProcess [

	UIProcess := [
		[WorldMorph doOneCycle.  Processor yield.  false] whileFalse: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess name: 'Morphic UI Process'.
	UIProcess resume
]

{ #category : #'ui requests' }
MorphicUIManager >> syntaxErrorNotificationDefaultAction: anException [
	"Handle a syntax error.
	Note, if there is no #syntaxErrorDebugger registered, it will be silently ignored"
	^Smalltalk tools using: #syntaxErrorDebugger do: [:tool | 
			tool open: (tool syntaxError: anException)]
]

{ #category : #'ui requests' }
MorphicUIManager >> systemNotificationDefaultAction: aNotification [

	aNotification messageText traceCr.
	aNotification resume.
]

{ #category : #'ui process' }
MorphicUIManager >> terminateUIProcess [
	UIProcess suspend; terminate.
	UIProcess := nil "?"
]

{ #category : #accessing }
MorphicUIManager >> theme [  
	^ Smalltalk ui theme
]

{ #category : #'ui process' }
MorphicUIManager >> uiProcess [
	" Answer the currently active UI process for morphic world.
	Client should check explicitly if #uiProcess answers nil or not (see other implementations)"

	^ UIProcess
]

{ #category : #'ui requests' }
MorphicUIManager >> unhandledErrorDefaultAction: anException [

	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."
	anException debug
]

{ #category : #'ui requests' }
MorphicUIManager >> warningDefaultAction: anException [

"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."
	UIManager default
				debugProcess: Processor activeProcess
				context: anException signalerContext
				label: 'Warning'
				fullView: false 
				notification: anException messageText, '\\Select Proceed to continue, or close this window to cancel the operation.' withCRs.
				
	anException resume.
]

{ #category : #'global state' }
MorphicUIManager >> world [
	^ self currentWorld 
]
