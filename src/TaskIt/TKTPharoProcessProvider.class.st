"
I am a process provider that creates Pharo processes using the standard #fork message
"
Class {
	#name : #TKTPharoProcessProvider,
	#superclass : #TKTProcessProvider,
	#category : #'TaskIt-Kernel'
}

{ #category : #'instance creation' }
TKTPharoProcessProvider >> createProcessDoing: aValuable [
	| toFork |
	toFork := aValuable isBlock
		ifTrue: [ aValuable ]
		ifFalse: [ [ aValuable value ] ].
	^ toFork fork
]

{ #category : #'instance creation' }
TKTPharoProcessProvider >> createProcessDoing: aValuable named: aName [
	| block |
	block := aValuable isMessageSend
		ifTrue: [ [ aValuable value ] ]
		ifFalse: [ aValuable ].
	^ block forkNamed: aName
]
