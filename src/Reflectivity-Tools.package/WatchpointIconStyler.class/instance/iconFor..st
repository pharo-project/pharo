defaults
iconFor: aNode
	^aNode watchpoint isRecording
		ifTrue: [ self iconProvider iconNamed: #testGreen]
		ifFalse: [ self iconProvider iconNamed: #testNotRunIcon ]
	