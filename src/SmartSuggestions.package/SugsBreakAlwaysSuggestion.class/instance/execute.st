execution
execute
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	self selectedNodeHasBreakpoint 
		ifFalse: [Breakpoint new node: context selectedNode; always; install]
		ifTrue: [ Breakpoint removeFrom: context selectedNode]