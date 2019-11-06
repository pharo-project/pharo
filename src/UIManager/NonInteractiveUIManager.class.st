"
This is a non-interactive UI manager, i.e. a UI manager which doesn't provides any kind of interactivity with users.

For most of requests, it throws an ErrorNonInteractive exception, which can be handled by various tools to do things differently when UI is not avaliable. For example:

response := [ UIManager default request: 'what is your name?' ] on: ErrorNonInteractive do: [:ex | ex resume: 'Mr. John Smith' ].

You can replace the default UI Manager with my instance in cases, when you need to guarantee that your task(s) will run in fully automated mode. This is useful for things like:
  - when image runs as a persistent application on a server
  - image runs headless from command-line with some batch scripts/commands


"
Class {
	#name : #NonInteractiveUIManager,
	#superclass : #CommandLineUIManager,
	#category : #'UIManager-Base'
}

{ #category : #testing }
NonInteractiveUIManager class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isHeadless and: [ Smalltalk isInteractive not ]
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> chooseDirectory: label from: dir [
	"Let the user choose a directory"
	^ self nonInteractiveWarning: (String streamContents: [ :s|
			s 
				nextPutAll: 'Interactive Directory Choice Request: ';
				nextPutAll: '[ '; nextPutAll: label asString; nextPutAll: ' ] from '; print: dir
		])
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."
	^ self chooseOrRequestFrom: aList values: aList lines: linesArray title: aString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."
	^ self chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> chooseFullFileNameMatching: patterns label: labelString [
	"Let the user choose a file matching the given patterns"
	^ self nonInteractiveWarning: (String streamContents: [ :s|
			s 
				nextPutAll: 'Interactive File Choice Request: ';
				nextPutAll: '[ '; nextPutAll: labelString asString; nextPutAll: ' ] ';
				print: patterns.
		])
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Choice Request: '; nextPutAll: aString; cr.
			labelList with: valueList do: [ :label :value| 
				s tab; nextPutAll: label asString; nextPutAll: ':'; tab; print: value; cr ]])
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: queryString [
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^ answer].
	
	^  self nonInteractiveWarning: 'Confirming: ', queryString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: queryString label: titleString [
	^  self nonInteractiveRequest: queryString title: titleString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol [
	^  self nonInteractiveRequest: queryString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: aString orCancel: cancelBlock [
	^  self nonInteractiveRequest: aString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [ 
	^  self nonInteractiveRequest: queryString
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: aSymbol [
	^  self nonInteractiveRequest: queryString
]

{ #category : #debug }
NonInteractiveUIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [
	^ self nonInteractiveWarning: 'Opening Debugger'
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"
	^self nonInteractiveWarning: (String streamContents: [ :s |
		s  nextPutAll: 'Editing Text: '.
		labelString ifNotNil: [ s nextPutAll: '[ '; nextPutAll: labelString; nextPutAll: ' ] '].
		s cr; nextPutAll: aText])
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> fontFromUser: priorFont [	
	^self nonInteractiveWarning: 'Font Choose Request'
]

{ #category : #'default actions' }
NonInteractiveUIManager >> lowSpaceWatcherDefaultAction: preemptedProcess [
	| ctx |
	"yeah.. space is low.. quit!! :)"
	
	ctx := preemptedProcess ifNotNil: [  preemptedProcess suspendedContext ].
	ctx ifNil: [ ctx := thisContext sender ]. 
	self quitFrom: ctx withMessage: 'Low space signal received.'.
	
	self error. "what else we can do? "
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> multiLineRequest: queryString initialAnswer: defaultAnswer answerHeight: answerHeight [
	^  self nonInteractiveRequest: queryString
]

{ #category : #errors }
NonInteractiveUIManager >> nonInteractive [
	^ ErrorNonInteractive signal
]

{ #category : #errors }
NonInteractiveUIManager >> nonInteractive: anException [
	^ ErrorNonInteractive signalForException: anException
]

{ #category : #'non-interactive' }
NonInteractiveUIManager >> nonInteractiveManager [
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it. 
	
	We already non-interactive. Just answer self
	"
	^ self
]

{ #category : #errors }
NonInteractiveUIManager >> nonInteractiveRequest: aStringOrText [
	^ self nonInteractiveRequest: aStringOrText title: nil
]

{ #category : #errors }
NonInteractiveUIManager >> nonInteractiveRequest: aStringOrText title: aTitle [
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Interactive Request: '.
			aTitle ifNotNil: [ s nextPutAll: '[ '; nextPutAll: aTitle asString; nextPutAll: ' ] ' ].
			s nextPutAll: aStringOrText asString.
		])
]

{ #category : #errors }
NonInteractiveUIManager >> nonInteractiveWarning: aWarnMessage [
	^ ProvideAnswerNotification signal: aWarnMessage
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> request: queryString initialAnswer: defaultAnswer [
	^ self request: queryString initialAnswer: defaultAnswer title: nil entryCompletion: nil. 
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> request: queryString initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	^ self request: queryString initialAnswer: defaultAnswer title: nil entryCompletion: anEntryCompletion. 
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle [
	^ self request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: nil. 
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [
	(ProvideAnswerNotification signal: aStringOrText)
		ifNotNil: [:answer | 
			^ answer = #default ifTrue: [ defaultAnswer ] ifFalse: [ answer ]].
		
	^ self nonInteractiveRequest: aStringOrText title: aTitle
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> requestPassword: queryString [
	^self nonInteractiveWarning: 'Requesting Password'.
]

{ #category : #'ui requests' }
NonInteractiveUIManager >> syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification [
	
	"display and log the syntax error"
	super syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification.
	
	"in noninteractive mode simply quit"
	^ self exitFailure
]
