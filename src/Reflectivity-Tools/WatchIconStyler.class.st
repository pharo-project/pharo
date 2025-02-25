"
I'm in charge to style an ast when there are watchpoints
"
Class {
	#name : 'WatchIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Watches',
	#package : 'Reflectivity-Tools',
	#tag : 'Watches'
}

{ #category : 'testing' }
WatchIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
WatchIconStyler >> highlightColor [
	^(Color purple alpha: 0.1)
]

{ #category : 'defaults' }
WatchIconStyler >> iconBlock: aNode [
	^[ WatchesWindowPresenter openOn: aNode watch]
]

{ #category : 'defaults' }
WatchIconStyler >> iconFor: aNode [
	^aNode watch isRecording
		ifTrue: [ self iconNamed: #testGreen]
		ifFalse: [ self iconNamed: #testNotRun ]
]

{ #category : 'defaults' }
WatchIconStyler >> iconLabel: aNode [

	^ aNode watch asString
]

{ #category : 'testing' }
WatchIconStyler >> shouldStyleNode: aNode [
	^aNode hasWatch
]
