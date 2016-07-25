as yet unclassified
gtChangeDebuggerActionsFor: aDebugger
	<gtDebuggingAction>
	
	^ self 
		changeDebuggerActionsForContext: aDebugger interruptedContext 
		excluding: aDebugger