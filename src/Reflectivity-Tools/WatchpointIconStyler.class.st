"
I'm in charge to style an ast when there are watchpoints
"
Class {
	#name : #WatchpointIconStyler,
	#superclass : #IconStyler,
	#category : #Reflectivity-Tools-Watchpoints
}

{ #category : #defaults }
WatchpointIconStyler >> highlightColor [
	^(Color purple alpha: 0.1)
]

{ #category : #defaults }
WatchpointIconStyler >> iconBlock: aNode [
	^[ WatchpointWindow openOn: aNode watchpoint]
]

{ #category : #defaults }
WatchpointIconStyler >> iconFor: aNode [
	^aNode watchpoint isRecording
		ifTrue: [ self iconNamed: #testGreen]
		ifFalse: [ self iconNamed: #testNotRunIcon ]
	
]

{ #category : #defaults }
WatchpointIconStyler >> iconLabel: aNode [

	^ aNode watchpoint asString.
]

{ #category : #testing }
WatchpointIconStyler >> shouldStyleNode: aNode [
	^aNode hasWatchpoint
]
