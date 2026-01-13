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

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> confirm: aString orCancel: cancelBlock [

	self error: 'No user response possible'
]

{ #category : 'ui requests' }
DummyUIManager >> edit: aText label: labelString accept: aBlockOrNil [

	^ nil
]

{ #category : 'ui requests' }
DummyUIManager >> inform: aString [
	"Nothing to be done here"
]

{ #category : 'default actions' }
DummyUIManager >> lowSpaceWatcherDefaultAction [

	Smalltalk logStdOutDuring: [ :logger |
			logger
				nextPutAll: '*** LOW SPACE ***';
				cr ]
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

	self error: 'No user response possible'
]
