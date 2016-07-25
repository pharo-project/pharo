execution
execute
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasExecutionCounter
		ifFalse: [ ExecutionCounter installOn: context selectedNode]
		ifTrue: [ ExecutionCounter removeFrom: context selectedNode]