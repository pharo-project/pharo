execution
execute
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	self selectedNodeHasBreakpoint 
		ifFalse: [ self breakpointForCondition ]
		ifTrue: [ Breakpoint removeFrom: context selectedNode]