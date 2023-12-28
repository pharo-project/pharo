"
I'm an alternative UIManager used to run an the image without GUI.  I redefine methods which requires user input as these requests are irrelevant in a headless environment. 
"
Class {
	#name : 'DummyUIManager',
	#superclass : 'UIManager',
	#classVars : [
		'ProgressBarEnabled'
	],
	#category : 'UIManager-Base',
	#package : 'UIManager',
	#tag : 'Base'
}

{ #category : 'ui requests' }
DummyUIManager >> chooseDirectory: label from: dir [

	^ nil
]

{ #category : 'ui requests' }
DummyUIManager >> chooseFrom: aList lines: linesArray title: aString [

	^ aList first
]

{ #category : 'ui requests' }
DummyUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [

	^ valueList first
]

{ #category : 'ui requests' }
DummyUIManager >> confirm: queryString [

	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^answer].

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer | ^ answer].

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> confirm: aString orCancel: cancelBlock [

	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer |
		^answer == #cancel ifTrue: [cancelBlock value] ifFalse: [answer]].

	self error: 'No user response possible'
]

{ #category : 'debug' }
DummyUIManager >> debugProcess: process context: context label: title fullView: bool notification: notificationString [
	Warning signal: 'Trying to debug the process ', title
]

{ #category : 'ui requests' }
DummyUIManager >> edit: aText label: labelString accept: aBlockOrNil [

	^ nil
]

{ #category : 'error handling' }
DummyUIManager >> handleError: anError log: shouldLog [

	self traceCr: '*** EXCEPTION ***'.
	self traceCr: anError.
	self trace: (String streamContents: [ :stream |
			 anError signalerContext errorReportOn: stream ]).
	Smalltalk exitFailure
]

{ #category : 'error handling' }
DummyUIManager >> handleWarning: aWarning [

	^ self handleError: aWarning
]

{ #category : 'ui requests' }
DummyUIManager >> inform: aString [
	"Nothing to be done here"
]

{ #category : 'ui requests' }
DummyUIManager >> informUserDuring: aBlock [

	aBlock value: DummySystemProgressItem new
]

{ #category : 'default actions' }
DummyUIManager >> lowSpaceWatcherDefaultAction [

	self traceCr: '*** LOW SPACE ***'
]

{ #category : 'ui requests' }
DummyUIManager >> multiLineRequest: queryString initialAnswer: defaultAnswer answerHeight: answerHeight [
]

{ #category : 'accessing' }
DummyUIManager >> progressBarEnabled [

  ^ ProgressBarEnabled ifNil: [ ProgressBarEnabled := true ]
]

{ #category : 'accessing' }
DummyUIManager >> progressBarEnabled: aBoolean [

  ProgressBarEnabled := aBoolean
]

{ #category : 'ui requests' }
DummyUIManager >> request: queryString initialAnswer: defaultAnswer [

	^ self
		  request: queryString
		  initialAnswer: defaultAnswer
		  title: 'Provide the following information'
		  entryCompletion: nil
]

{ #category : 'ui requests' }
DummyUIManager >> request: aStringOrText initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [

	^ self request: aStringOrText initialAnswer: defaultAnswer title: 'Information Required' entryCompletion: anEntryCompletion
]

{ #category : 'ui requests' }
DummyUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [

	(ProvideAnswerNotification signal: aStringOrText) ifNotNil: [:answer |
		^ answer == #default ifTrue: [defaultAnswer] ifFalse: [answer]].

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> requestPassword: queryString [

	^ self request: queryString initialAnswer: ''
]

{ #category : 'default actions' }
DummyUIManager >> traceCr: aString [

	Smalltalk logStdOutDuring: [:logger |
		logger nextPutAll: aString;
				 cr ]
]

{ #category : 'default actions' }
DummyUIManager >> unhandledErrorDefaultAction: anException [
	self traceCr: '*** EXCEPTION ***'.
	self traceCr: anException.
	self
		trace:
			(String
				streamContents: [ :stream | anException signalerContext errorReportOn: stream ]).
	Smalltalk exitFailure
]
