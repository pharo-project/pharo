"
UIManager is a dispatcher for various UI requests.
"
Class {
	#name : #UIManager,
	#superclass : #Object,
	#classVars : [
		'Default'
	],
	#category : #'UIManager-Base'
}

{ #category : #initialization }
UIManager class >> default [
	^ Default
		ifNil: [ 
			"Note: The way the following is phrased ensures that you can always make 'more specific' managers merely by subclassing a tool builder and implementing a more specific way of reacting to #isActiveManager. For example, a BobsUIManager can subclass MorphicUIManager and will be considered before the parent (generic MorphicUIManager)."
			self allSubclasses
				detect: [ :any | any isActiveManager and: [ any subclasses noneSatisfy: [ :sub | sub isActiveManager ] ] ]
				ifFound: [ :mgrClass | Default := mgrClass new ] ]
]

{ #category : #initialization }
UIManager class >> default: aUIManager [

	Default deactivate.
	Default := aUIManager.
	Default activate.
]

{ #category : #initialization }
UIManager class >> forCurrentSystemConfiguration [
	^ (self allSubclasses
			detect: [ :any | 
				any isValidForCurrentSystemConfiguration
					and: [ any subclasses noneSatisfy: #isValidForCurrentSystemConfiguration ] ]
			ifNone: [ NonInteractiveUIManager ]) new
]

{ #category : #initialization }
UIManager class >> isActiveManager [
	"Answer whether I should act as the active ui manager"
	^false
]

{ #category : #testing }
UIManager class >> isValidForCurrentSystemConfiguration [

	^ false
]

{ #category : #utilities }
UIManager class >> nonInteractiveDuring: aBlock [
	| currentManager |
	currentManager := self default.
	currentManager nonInteractiveManager.
	
	aBlock ensure: [ currentManager beDefault ]
]

{ #category : #'ui TEasilyThemed' }
UIManager >> abort: aStringOrText [
	"Open an error dialog."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> abort: aStringOrText title: aString [
	"Open an error dialog."

	^self subclassResponsibility 
]

{ #category : #private }
UIManager >> activate [
]

{ #category : #'ui TEasilyThemed' }
UIManager >> alert: aStringOrText [
	"Open an alert dialog."
	
	^ self subclassResponsibility
]

{ #category : #'ui TEasilyThemed' }
UIManager >> alert: aStringOrText title: aString [
	"Open an alert dialog."
	
	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> alert: aStringOrText title: aString configure: aBlock [
	"Open an alert dialog.
	Configure the dialog with the 1 argument block
	before opening modally."

	^self subclassResponsibility 
]

{ #category : #private }
UIManager >> beDefault [

	self class default: self.
]

{ #category : #display }
UIManager >> checkForNewDisplaySize [	
	
	self subclassResponsibility 
]

{ #category : #'ui requests' }
UIManager >> chooseDirectory [
	"Let the user choose a directory"
	^self chooseDirectoryFrom: FileSystem workingDirectory
]

{ #category : #'ui requests' }
UIManager >> chooseDirectory: label [
	"Let the user choose a directory"
	^self chooseDirectory: label from: FileSystem workingDirectory
]

{ #category : #'ui requests' }
UIManager >> chooseDirectory: label from: dir [
	"Let the user choose a directory"
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> chooseDirectoryFrom: dir [
	"Let the user choose a directory"
	^self chooseDirectory: nil from: dir
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList [
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseFrom: aList lines: #()
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList lines: linesArray [
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseFrom: aList lines: linesArray title: ''
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList lines: linesArray message: messageString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList lines: linesArray message: messageString title: ''
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."
	
	^self
		chooseFrom: aList
		lines: linesArray
		title: (aString 
				ifEmpty: [messageString]
				ifNotEmpty: [aString, String cr, messageString])
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList message: messageString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList lines: #() message: messageString
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList message: messageString title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList lines: #() message: messageString title: aString
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^self chooseFrom: aList lines: #() title: aString
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: labelList values: valueList [
	"Choose an item from the given list. Answer the selected item."
	^self chooseFrom: labelList values: valueList lines: #()
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: labelList values: valueList lines: linesArray [
	"Choose an item from the given list. Answer the selected item."
	^self chooseFrom: labelList values: valueList lines: linesArray title: ''
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList values: valueList lines: linesArray message: messageString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList values: valueList lines: linesArray message: messageString title: ''
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: labelList values: valueList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."
	
	^self
		chooseFrom: labelList
		values: valueList 
		lines: linesArray
		title: (aString 
				ifEmpty: [messageString]
				ifNotEmpty: [aString, String cr, messageString])
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList values: valueList message: messageString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList values: valueList lines: #() message: messageString
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: aList values: valueList message: messageString title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	
	^self chooseFrom: aList values: valueList lines: #() message: messageString title: aString
]

{ #category : #'ui requests' }
UIManager >> chooseFrom: labelList values: valueList title: aString [
	"Choose an item from the given list. Answer the selected item."
	^self chooseFrom: labelList values: valueList lines: #() title: aString
]

{ #category : #'ui requests' }
UIManager >> chooseFullFileNameMatching: patterns [
	"Let the user choose a file matching the given patterns"
	^self chooseFullFileNameMatching: patterns label: nil
]

{ #category : #'ui requests' }
UIManager >> chooseFullFileNameMatching: patterns label: labelString [
	"Let the user choose a file matching the given patterns"
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: queryString label: titleString [
	
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: aString orCancel: cancelBlock [
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [ 
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice, false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	Default should be one of #yes, #no or #cancel to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."
	
	^self subclassResponsibility
]

{ #category : #private }
UIManager >> deactivate [
]

{ #category : #debug }
UIManager >> debugProcess: process context: context label: title fullView: bool [

	self debugProcess: process context: context label: title fullView: bool notification: nil
]

{ #category : #debug }
UIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [
	self subclassResponsibility
]

{ #category : #'ui process' }
UIManager >> defer: aBlock [
	" Evaluate the given Block in the UI thread as soon as there is nothing scheduled. Evaluate immediately when there is no UI"
	aBlock value
]

{ #category : #'ui TEasilyThemed' }
UIManager >> deny: aStringOrText [
	"Open a denial dialog."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> deny: aStringOrText title: aString [
	"Open a denial dialog."

	^self subclassResponsibility 
]

{ #category : #display }
UIManager >> displayProgress: titleString from: minVal to: maxVal during: workBlock [
	"SystemProgressMorph show: titleString from: minVal to:  during: "

	^ workBlock asJob
			title: titleString;
			min: minVal;
			max: maxVal;
			run.
]

{ #category : #'ui requests' }
UIManager >> edit: aText [
	"Open an editor on the given string/text"
	^self edit: aText label: nil
]

{ #category : #'ui requests' }
UIManager >> edit: aText label: labelString [
	"Open an editor on the given string/text"
	^self edit: aText label: labelString accept: nil
]

{ #category : #'ui requests' }
UIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> fontFromUser: priorFont [	
	
	self subclassResponsibility
]

{ #category : #'error handling' }
UIManager >> handleError: anError [
	
	self handleError: anError log: true
]

{ #category : #'error handling' }
UIManager >> handleError: anError log: shouldLog [

	self subclassResponsibility
]

{ #category : #accessing }
UIManager >> headlessManager [
	" Answer an instance of headless manager, which will be used when image runs headless and non-interactive.
	  We put it here, so subclasses can override it. "
	^ CommandLineUIManager replacing: self
]

{ #category : #'ui requests' }
UIManager >> inform: aString [
	"Display a message for the user to read and then dismiss"
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> informUser: aString during: aBlock [
	"Display a message above (or below if insufficient room) the cursor 
	during execution of the given block.
		UIManager default informUser: 'Just a sec!' during: [(Delay forSeconds: 1) wait].
	"
	^self informUserDuring: [:bar | bar label: aString. aBlock value].
]

{ #category : #'ui requests' }
UIManager >> informUserDuring: aBlock [
	"Display a message above (or below if insufficient room) the cursor 
	during execution of the given block.
		UIManager default informUserDuring:[:bar|
			#(one two three) do:[:info|
				bar label: info.
				(Delay forSeconds: 1) wait]]"
	^self subclassResponsibility
]

{ #category : #'ui process' }
UIManager >> interruptName: labelString preemptedProcess: theInterruptedProcess [
	"Create a Notifier on the active scheduling process with the given label."

	^ self error: 'Cannot perform a given request'
]

{ #category : #'error handling' }
UIManager >> logError: anError [

	Smalltalk
		logError: anError messageText
		inContext: anError signalerContext.
]

{ #category : #'default actions' }
UIManager >> lowSpaceWatcherDefaultAction: preemptedProcess [
]

{ #category : #'ui requests' }
UIManager >> merge: merger informing: aString [

	self subclassResponsibility.
]

{ #category : #'ui requests' }
UIManager >> multiLineRequest: queryString initialAnswer: defaultAnswer answerHeight: answerHeight [
	"Create a multi-line instance of me whose question is queryString with
	the given initial answer. Invoke it centered at the given point, and
	answer the string the user accepts.  Answer nil if the user cancels.  An
	empty string returned means that the ussr cleared the editing area and
	then hit 'accept'.  Because multiple lines are invited, we ask that the user
	use the ENTER key, or (in morphic anyway) hit the 'accept' button, to 
	submit; that way, the return key can be typed to move to the next line."
	^self subclassResponsibility
]

{ #category : #display }
UIManager >> newDisplayDepthNoRestore: pixelSize [	
	
	self subclassResponsibility 
]

{ #category : #'ui requests' }
UIManager >> newMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."
	
	^ self subclassResponsibility
]

{ #category : #'non-interactive' }
UIManager >> nonInteractiveManager [
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it. "
	^ NonInteractiveUIManager replacing: self
]

{ #category : #events }
UIManager >> onEventSensorStartup: anEventSensor [


]

{ #category : #'default actions' }
UIManager >> onFontsChanged [

	
]

{ #category : #events }
UIManager >> onPrimitiveError: aString [	
]

{ #category : #'ui requests' }
UIManager >> openComparisionFrom: targetMethodSource
					to: originalMethodSource
					belongingTo: aClass
					from: aChange [ 
	^ self subclassResponsibility				
]

{ #category : #'ui TEasilyThemed' }
UIManager >> proceed: aStringOrText [
	"Open a proceed dialog."
	
	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> proceed: aStringOrText title: aString [
	"Open a proceed dialog and answer true if not cancelled, false otherwise."
	
	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> question: aStringOrText [
	"Open a question dialog."
	
	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> question: aStringOrText title: aString [
	"Open a question dialog and answer true if yes,
	false if no and nil if cancelled."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> questionWithoutCancel: aStringOrText [
	"Open a question dialog."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> questionWithoutCancel: aStringOrText title: aString [
	"Open a question dialog and answer true if yes,
	false if no and nil if cancelled."

	^self subclassResponsibility 
]

{ #category : #'ui requests' }
UIManager >> request: queryString [ 
	"Create an instance of me whose question is queryString. Invoke it 
	centered at the cursor, and answer the string the user accepts. Answer 
	the empty string if the user cancels."
	^self request: queryString initialAnswer: ''
]

{ #category : #'ui requests' }
UIManager >> request: queryString entryCompletion: anEntryCompletion [
	"Create an instance of me whose question is queryString. Invoke it 
	centered at the cursor, and answer the string the user accepts. Answer 
	the empty string if the user cancels."
	^self request: queryString initialAnswer: '' entryCompletion: anEntryCompletion
]

{ #category : #'ui requests' }
UIManager >> request: queryString initialAnswer: defaultAnswer [
	"Create an instance of me whose question is queryString with the given initial answer. Answer the string the user accepts. Answer the empty string if the user cancels."

	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> request: queryString initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	"Create an instance of me whose question is queryString with the given 
	initial answer. Invoke it centered at the given point, and answer the 
	string the user accepts. Answer the empty string if the user cancels."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle [
	"Create an instance of me whose question is queryString with the given 
	initial answer. Invoke it centered at the given point, and answer the 
	string the user accepts. Answer the empty string if the user cancels."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [
	"Create an instance of me whose question is queryString with the given 
	initial answer. Invoke it centered at the given point, and answer the 
	string the user accepts. Answer the empty string if the user cancels."
	^self subclassResponsibility
]

{ #category : #'ui requests' }
UIManager >> requestPassword: queryString [
	"Create an instance of me whose question is queryString. Invoke it centered
	at the cursor, and answer the string the user accepts. Answer the empty 
	string if the user cancels."
	^self subclassResponsibility
]

{ #category : #display }
UIManager >> restoreDisplay [	
	
	self subclassResponsibility 
]

{ #category : #display }
UIManager >> restoreDisplayAfter: aBlock [	
	
	self subclassResponsibility 
]

{ #category : #display }
UIManager >> showWaitCursorWhile: aBlock [
	| process |
	process := [ [ true ] "loop until the end of aBlock processing"
		whileTrue: [ 
			Smalltalk logStdErrorDuring: [ :stream |
				"Wait wheel in ASCII art"
				'\|/-'
					do: [ :char |
						stream << char.
						0.1 second wait.
						stream << Character backspace ] ] ] ] fork.
	[ aBlock value ]
		ensure: [ process terminate ]
]

{ #category : #'ui process' }
UIManager >> spawnNewProcess [

	"do nothing by default"
]

{ #category : #'ui process' }
UIManager >> spawnNewProcessIfThisIsUI: suspendedProcess [

	self uiProcess == suspendedProcess ifTrue: [
		self spawnNewProcess.
		^true
	].
	^false		"no new process was created"

]

{ #category : #'default actions' }
UIManager >> syntaxErrorNotificationDefaultAction: anException [

	^ self subclassResponsibility 
]

{ #category : #'ui process' }
UIManager >> terminateUIProcess [
	"do nothing by default "
]

{ #category : #'ui TEasilyThemed' }
UIManager >> textEntry: aStringOrText [
	"Open a text entry dialog."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> textEntry: aStringOrText title: aString [
	"Open a text entry dialog."

	^self subclassResponsibility 
]

{ #category : #'ui TEasilyThemed' }
UIManager >> textEntry: aStringOrText title: aString entryText: defaultEntryText [
	"Open a text entry dialog."

	^self subclassResponsibility 
]

{ #category : #'default actions' }
UIManager >> unhandledErrorDefaultAction: anException [

	"Provide a default handling for unhandled error. 
	We should never reach this code, because it should be overridden by subclasses. 
	(but we can't put 'self subclassResponsibility' , because it will cause infinite loop,
	in case if current ui manager having no override of this method).
	This method is only for documentation purposes."
		
	Smalltalk snapshot: false andQuit: true.
]

{ #category : #'default actions' }
UIManager >> warningDefaultAction: aWarning [

	"Handle warning notification. Should be overidden by subclass"
	
	self subclassResponsibility  
]
