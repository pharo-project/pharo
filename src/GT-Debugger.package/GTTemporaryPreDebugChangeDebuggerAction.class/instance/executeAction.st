actions
executeAction
	| currentDebugger currentSession |
	
	"Close the spec PreDebugWindow and debugger"
	currentDebugger := self preDebugWindow debugger.
	self preDebugWindow debugger: nil.
	self preDebugWindow close.
	currentSession := currentDebugger session.
	currentDebugger session: nil.
	currentDebugger clear. "clear not close, as at this point there is no open window to close"
	
	GTGenericStackDebugger debugSession: currentSession