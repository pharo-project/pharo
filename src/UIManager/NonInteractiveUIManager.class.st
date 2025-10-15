"
This is a non-interactive UI manager, i.e. a UI manager which doesn't provides any kind of interactivity with users.

For most of requests, it throws an ErrorNonInteractive exception, which can be handled by various tools to do things differently when UI is not avaliable. For example:

response := [ UIManager default request: 'what is your name?' ] on: ErrorNonInteractive do: [:ex | ex resume: 'Mr. John Smith' ].

You can replace the default UI Manager with my instance in cases, when you need to guarantee that your task(s) will run in fully automated mode. This is useful for things like:
  - when image runs as a persistent application on a server
  - image runs headless from command-line with some batch scripts/commands


"
Class {
	#name : 'NonInteractiveUIManager',
	#superclass : 'UIManager',
	#instVars : [
		'doNotQuitOnRestart'
	],
	#classVars : [
		'SnapshotErrorImage'
	],
	#category : 'UIManager-Base',
	#package : 'UIManager',
	#tag : 'Base'
}

{ #category : 'testing' }
NonInteractiveUIManager class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isInteractive not
]

{ #category : 'accessing' }
NonInteractiveUIManager class >> snapshotErrorImage [
	^ SnapshotErrorImage == true
]

{ #category : 'accessing' }
NonInteractiveUIManager class >> snapshotErrorImage: aBoolean [
	SnapshotErrorImage := aBoolean
]

{ #category : 'settings' }
NonInteractiveUIManager class >> uiSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #nonInteractive)
		label: 'Headless mode';
		parent: #pharoSystem;
		description: 'Settings related to mode without UI';
		with: [
			(aBuilder setting: #snapshotErrorImage)
				label: 'Snapshot before quitting' ;
				default: false;
				target: NonInteractiveUIManager;
				description: 'On unhandled exception, save a new version of the image before quitting']
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> abort: aStringOrText [

	self abort: aStringOrText title: 'Error'
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> abort: aStringOrText title: aString [

	self logTitle: aString andDescription: aStringOrText to: self stderr
]

{ #category : 'private' }
NonInteractiveUIManager >> activate [
	super activate.
	NonInteractiveTranscript stdout install
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> alert: aStringOrText [

	self abort: aStringOrText title: 'Alert'
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> alert: aStringOrText title: aString [

	self logTitle: aString andDescription: aStringOrText to: self stdout
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> alert: aStringOrText title: aString configure: aBlock [
	"Ignore the block, because we don't create a dialog, so there is nothing to configure."

	self alert: aStringOrText title: aString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> chooseDirectory: label from: dir [
	"Let the user choose a directory"
	^ self nonInteractiveWarning: (String streamContents: [ :s|
			s
				nextPutAll: 'Interactive Directory Choice Request: ';
				nextPutAll: '[ '; nextPutAll: label asString; nextPutAll: ' ] from '; print: dir
		])
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^ self chooseOrRequestFrom: aList values: aList lines: linesArray title: aString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."
	^ self chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> chooseFullFileNameMatching: patterns label: labelString [
	"Let the user choose a file matching the given patterns"
	^ self nonInteractiveWarning: (String streamContents: [ :s|
			s
				nextPutAll: 'Interactive File Choice Request: ';
				nextPutAll: '[ '; nextPutAll: labelString asString; nextPutAll: ' ] ';
				print: patterns.
		])
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Choice Request: '; nextPutAll: aString; cr.
			labelList with: valueList do: [ :label :value|
				s tab; nextPutAll: label asString; nextPutAll: ':'; tab; print: value; cr ]])
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: queryString [

	^  self nonInteractiveWarning: 'Confirming: ', queryString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: queryString label: titleString [
	^  self nonInteractiveRequest: queryString title: titleString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol [
	^  self nonInteractiveRequest: queryString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: aString orCancel: cancelBlock [
	^  self nonInteractiveRequest: aString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [
	^  self nonInteractiveRequest: queryString
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol [
	^  self nonInteractiveRequest: queryString
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> deny: aStringOrText [
	"Open a denial dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> deny: aStringOrText title: aString [
	"Open a denial dialog."
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"
	^self nonInteractiveWarning: (String streamContents: [ :s |
		s  nextPutAll: 'Editing Text: '.
		labelString ifNotNil: [ s nextPutAll: '[ '; nextPutAll: labelString; nextPutAll: ' ] '].
		s cr; nextPutAll: aText])
]

{ #category : 'utilities' }
NonInteractiveUIManager >> exitFailure [
	[
		self class snapshotErrorImage ifTrue: [
			doNotQuitOnRestart := true.
			"make a new image version snapshot before leaving"
			Smalltalk saveAsNewVersion  ].
	] ensure: [
		doNotQuitOnRestart ifFalse: [ Smalltalk exitFailure ].
		doNotQuitOnRestart := false.
	]
]

{ #category : 'error handling' }
NonInteractiveUIManager >> handleError: anError log: shouldLog [

	self
		quitFrom: anError signalerContext
		withMessage: anError description
]

{ #category : 'error handling' }
NonInteractiveUIManager >> handleNotification: aNotification [

	Smalltalk logStdOutDuring: [:logger |
		logger nextPutAll: aNotification class name;
				 nextPutAll: ': ';
				 nextPutAll: aNotification messageText;
				 cr ].

	aNotification resume
]

{ #category : 'error handling' }
NonInteractiveUIManager >> handleWarning: aWarning [
	| logBlock |
	"Pass all warnings, but log them"

	logBlock := [:logger |
		logger
			cr;
			nextPutAll: '*** Warning: ';
			nextPutAll: aWarning description;
			cr ].

	Smalltalk logDuring: logBlock.
	self logYellowDuring: logBlock.

	aWarning resume
]

{ #category : 'handle debug requests' }
NonInteractiveUIManager >> handleWarningDebugRequest: aDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [

	anOupsDebuggerSystem
		warningRequest: aDebugRequest
		withUIManager: self
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> inform: aString [

	| logBlock |
	logBlock := [:logger |
		logger cr;
			nextPutAll: (String new: 79 withAll: $= ); cr;
			nextPutAll: 'Notice: ';
			nextPutAll: aString; cr;
			nextPutAll: (String new: 79 withAll: $= ); cr].

	Smalltalk logDuring: logBlock
]

{ #category : 'initialization' }
NonInteractiveUIManager >> initialize [
	doNotQuitOnRestart := false
]

{ #category : 'utilities' }
NonInteractiveUIManager >> logColored: anAnsiiColorCode during: aBlock [

	Smalltalk logStdErrorDuring: [:stderr |
		stderr
			nextPut: Character escape;
			nextPut: $[; nextPutAll: anAnsiiColorCode; nextPut: $m.

		aBlock value: stderr.

		stderr nextPut: Character escape; nextPutAll: '[0m'
	]
]

{ #category : 'private' }
NonInteractiveUIManager >> logTitle: aString andDescription: aStringOrText to: aStream [

	aStream
		nextPutAll: aString;
		nextPutAll: ': ';
		nextPutAll: aStringOrText asString;
		cr
]

{ #category : 'utilities' }
NonInteractiveUIManager >> logYellowDuring: aBlock [

	^ self logColored: '33' during: aBlock
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> multiLineRequest: queryString initialAnswer: defaultAnswer answerHeight: answerHeight [
	^  self nonInteractiveRequest: queryString
]

{ #category : 'non-interactive' }
NonInteractiveUIManager >> nonInteractiveManager [
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it.

	We already non-interactive. Just answer self
	"
	^ self
]

{ #category : 'errors' }
NonInteractiveUIManager >> nonInteractiveRequest: aStringOrText [
	^ self nonInteractiveRequest: aStringOrText title: nil
]

{ #category : 'errors' }
NonInteractiveUIManager >> nonInteractiveRequest: aStringOrText title: aTitle [
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Interactive Request: '.
			aTitle ifNotNil: [ s nextPutAll: '[ '; nextPutAll: aTitle asString; nextPutAll: ' ] ' ].
			s nextPutAll: aStringOrText asString.
		])
]

{ #category : 'errors' }
NonInteractiveUIManager >> nonInteractiveWarning: aWarnMessage [
	^ self
]

{ #category : 'events' }
NonInteractiveUIManager >> onPrimitiveError: aString [

	" log error and quit "
	^ self quitFrom: thisContext sender withMessage: aString
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> proceed: aStringOrText [
	"Open a proceed dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> proceed: aStringOrText title: aString [
	"Open a proceed dialog and answer true if not cancelled, false otherwise."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> question: aStringOrText [
	"Open a question dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> question: aStringOrText title: aString [
	"Open a question dialog and answer true if yes,
	false if no and nil if cancelled."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> questionWithoutCancel: aStringOrText [
	"Open a question dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> questionWithoutCancel: aStringOrText title: aString [
	"Open a question dialog and answer true if yes,
	false if no and nil if cancelled."
]

{ #category : 'utilities' }
NonInteractiveUIManager >> quitFrom: aContext withMessage: aString [

	" log error and quit "
	[ Smalltalk
		logError: aString
		inContext: aContext.

	" Print stacks of all current processes "

	Smalltalk logDuring: [:logger |
		logger nextPutAll: 'Processes and their stacks: ';cr.

		Process allInstances do: [:each | | ctx |
			logger nextPutAll: 'Process: '; print: each; cr; nextPutAll: '  stack:'; cr; cr.

			ctx := each isActive ifTrue: [ thisContext sender ] ifFalse: [ each suspendedContext ].
			ctx ifNotNil: [
				(ctx stackOfSize: 20) do: [:s | logger print: s; cr ]].
			logger nextPutAll: '------------------------------'; cr; cr.
		]].
	] ensure: [ self exitFailure ]
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> request: queryString initialAnswer: defaultAnswer [

	^ self
		  request: queryString
		  initialAnswer: defaultAnswer
		  title: 'Provide the following information'
		  entryCompletion: nil
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> request: queryString initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	^ self request: queryString initialAnswer: defaultAnswer title: nil entryCompletion: anEntryCompletion.
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle [

	^ self
		  request: aStringOrText
		  initialAnswer: defaultAnswer
		  title: aTitle
		  entryCompletion: nil
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [

	^ self nonInteractiveRequest: aStringOrText title: aTitle
]

{ #category : 'ui requests' }
NonInteractiveUIManager >> requestPassword: queryString [
	^self nonInteractiveWarning: 'Requesting Password'
]

{ #category : 'accessing' }
NonInteractiveUIManager >> stderr [

	^ Stdio stderr
]

{ #category : 'accessing' }
NonInteractiveUIManager >> stdin [

	^ Stdio stdin
]

{ #category : 'accessing' }
NonInteractiveUIManager >> stdout [

	^ Stdio stdout
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> textEntry: aStringOrText [
	"Open a text entry dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> textEntry: aStringOrText title: aString [
	"Open a text entry dialog."
]

{ #category : 'ui TEasilyThemed' }
NonInteractiveUIManager >> textEntry: aStringOrText title: aString entryText: defaultEntryText [
	"Open a text entry dialog."
]
