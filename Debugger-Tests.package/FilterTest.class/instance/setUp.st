running
setUp

	context := [ (Set with: 1 with: 2) collect: [ :e | e * 2 ]. self halt ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
	session := process newDebugSessionNamed: 'test session' startedAt: context.
	nilContext := [ self isNil ] asContext.
	otherProcess := Process 
		forContext: nilContext 
		priority: Processor userInterruptPriority.
	otherSession := otherProcess newDebugSessionNamed: 'other session' startedAt: nilContext.
	otherSession stepInto