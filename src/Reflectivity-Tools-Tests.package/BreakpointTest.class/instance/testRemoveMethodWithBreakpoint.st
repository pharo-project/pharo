tests
testRemoveMethodWithBreakpoint
	cls compile: 'dummy ^42'.
	self assert: Breakpoint all isEmpty.
	Breakpoint new
		node: (cls >> #dummy) ast;
		once;
		install.
	self assert: (cls >> #dummy) hasBreakpoint.
	cls removeSelector: #dummy.
	self
		assert: cls methods isEmpty;
		assert: Breakpoint all isEmpty