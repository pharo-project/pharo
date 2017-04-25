tests
testRemoveClassWithBreakpoint
	cls compile: 'dummy ^42'.
	self assert: Breakpoint all isEmpty.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeFromSystem.
	self assert: Breakpoint all isEmpty