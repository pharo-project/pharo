actions code
codeContextMenuDebuggingActions

	^ self session class 
		debuggingActionsForPragmas: #(contextMenuCodeDebuggingAction) 
		for: self