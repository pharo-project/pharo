execution
execute
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasWatchpoint
		ifFalse: [
			(UIManager default request: 'Name for Watchpoint') ifNotNil: [:tagname | 
				Watchpoint in: context selectedNode named: tagname
			]
		]
		ifTrue: [ Watchpoint removeFrom: context selectedNode]