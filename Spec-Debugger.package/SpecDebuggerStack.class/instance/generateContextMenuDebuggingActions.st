actions lookup
generateContextMenuDebuggingActions

	actionsCache ifNotNil: [  ^ actionsCache ].
	"Cache the actions as this method is called multiple times: 
	two times when the menu is created and one time when the registering shortcuts."
	^ actionsCache := self session class 
		debuggingActionsForPragmas: self contextMenuDebuggingActionsPragmas 
		for: self debugger