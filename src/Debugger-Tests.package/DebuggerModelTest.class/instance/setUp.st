running
setUp

	context := [ Set new ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
		
	session:= process newDebugSessionNamed: 'test session' startedAt: context.