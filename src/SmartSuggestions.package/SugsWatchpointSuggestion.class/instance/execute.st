execution
execute
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasWatchpoint
		ifFalse: [ Watchpoint in: context selectedNode ]
		ifTrue: [ Watchpoint removeFrom: context selectedNode]