actions
openFullDebugger
	| currentDebugger |
	
	currentDebugger := self debugger.
	self debugger: nil.
	self close.
	currentDebugger 
		open