"
I'm an alternative UIManager used to run an the image without GUI.  I redefine methods which requires user input as these requests are irrelevant in a headless environment. 
"
Class {
	#name : #DummyUIManager,
	#superclass : #UIManager,
	#classVars : [
		'ProgressBarEnabled'
	],
	#category : #'UIManager-Base'
}

{ #category : #'ui requests' }
DummyUIManager >> chooseDirectory: label from: dir [

	^ nil
]

{ #category : #'ui requests' }
DummyUIManager >> chooseFrom: aList lines: linesArray title: aString [

	^ aList first
]

{ #category : #'ui requests' }
DummyUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [

	^ valueList first
]

{ #category : #'ui requests' }
DummyUIManager >> confirm: queryString [

	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^answer].

	self error: 'No user response possible'
]

{ #category : #'ui requests' }
DummyUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer | ^ answer].

	self error: 'No user response possible'
]

{ #category : #'ui requests' }
DummyUIManager >> confirm: aString orCancel: cancelBlock [

	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer |
		^answer == #cancel ifTrue: [cancelBlock value] ifFalse: [answer]].

	self error: 'No user response possible'
]

{ #category : #debug }
DummyUIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [
	Warning signal: 'Trying to debug the process ', title
]

{ #category : #'ui requests' }
DummyUIManager >> edit: aText label: labelString accept: aBlockOrNil [

	^ nil
]

{ #category : #'ui requests' }
DummyUIManager >> fontFromUser: priorFont [

	self error: 'No user response possible'
]

{ #category : #'error handling' }
DummyUIManager >> handleError: anError log: shouldLog [

	"NOOP"
]

{ #category : #'ui requests' }
DummyUIManager >> inform: aString [
	"Nothing to be done here"
]

{ #category : #'ui requests' }
DummyUIManager >> informUserDuring: aBlock [

	aBlock value: DummySystemProgressItem new.
]

{ #category : #'default actions' }
DummyUIManager >> lowSpaceWatcherDefaultAction [

	self traceCr: '*** LOW SPACE ***'.

]

{ #category : #'ui requests' }
DummyUIManager >> merge: merger informing: aString [
]

{ #category : #'ui requests' }
DummyUIManager >> multiLineRequest: queryString initialAnswer: defaultAnswer answerHeight: answerHeight [
]

{ #category : #accessing }
DummyUIManager >> progressBarEnabled [

  ^ ProgressBarEnabled ifNil: [ ProgressBarEnabled := true ].
]

{ #category : #accessing }
DummyUIManager >> progressBarEnabled: aBoolean [

  ProgressBarEnabled := aBoolean
]

{ #category : #'ui requests' }
DummyUIManager >> request: queryString initialAnswer: defaultAnswer [ 

	^ self request: queryString initialAnswer: defaultAnswer title: 'Provide the following information' entryCompletion: nil

]

{ #category : #'ui requests' }
DummyUIManager >> request: aStringOrText initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	
	^ self request: aStringOrText initialAnswer: defaultAnswer title: 'Information Required' entryCompletion: anEntryCompletion

]

{ #category : #'ui requests' }
DummyUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [

	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^ answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].
	
	self error: 'No user response possible'
]

{ #category : #'ui requests' }
DummyUIManager >> requestPassword: queryString [
	
	^ self request: queryString initialAnswer: ''
]

{ #category : #display }
DummyUIManager >> restoreDisplay [
]

{ #category : #display }
DummyUIManager >> restoreDisplayAfter: aBlock [

	aBlock value.
	self restoreDisplay.
]

{ #category : #'default actions' }
DummyUIManager >> syntaxErrorNotificationDefaultAction: anException [
	self traceCr: '*** SYNTAX ERROR ***'.
	self traceCr: anException.
	self
		traceCr: (thisContext stack first: (20 min: thisContext stack size)).
	anException return
]

{ #category : #'default actions' }
DummyUIManager >> unhandledErrorDefaultAction: anException [
	self traceCr: '*** EXCEPTION ***'.
	self traceCr: anException.
	self
		trace:
			(String
				streamContents: [ :stream | anException signalerContext errorReportOn: stream ]).
	Smalltalk exitFailure
]

{ #category : #'default actions' }
DummyUIManager >> warningDefaultAction: anException [

	^ self unhandledErrorDefaultAction: anException
]
