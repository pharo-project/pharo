actions
debug: aContext using: aDebuggerClass
	| currentSession newDebugger |
	
	currentSession := self session.
	self detachSession.
	self browser close.
	currentSession updateContextTo: aContext.
	newDebugger := aDebuggerClass on: (aDebuggerClass spanNewSessionFrom: currentSession).
	newDebugger openWithFullView.
	newDebugger window window "Hack to get this working with SpecDebugger"
		position: self window position;
		extent: self window extent.
	^ newDebugger
		