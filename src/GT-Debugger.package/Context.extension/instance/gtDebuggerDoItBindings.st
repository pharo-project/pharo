*GT-Debugger
gtDebuggerDoItBindings
	^ (self method isDoIt and: [ self receiver isNil ]) 
		ifTrue: [ self method literals select: [ :each | 
			each class = WorkspaceVariable ] ]
		ifFalse: [ #() ]