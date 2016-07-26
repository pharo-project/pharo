window creation and deletion
unregisterWindowWithId: windowId

	WindowMap removeKey: windowId.
	
	"Does it make sense to keep the SDL event loop process running even if no OSWindow is currently open ?"
	"WindowMap ifEmpty: [ 
		EventLoopProcess terminate.
		EventLoopProcess := nil.
	]."