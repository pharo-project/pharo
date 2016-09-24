defaults
iconFor: aNode
	^aNode watchpoint isRecording
		ifTrue: [ self iconNamed: #testGreen]
		ifFalse: [ self iconNamed: #testNotRunIcon ]
	