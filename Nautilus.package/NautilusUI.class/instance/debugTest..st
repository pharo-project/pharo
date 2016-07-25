debugging actions
debugTest: aMethod
	| breakpoint  |
	breakpoint := Breakpoint new
		node: aMethod ast;
		install.
	[aMethod methodClass run: aMethod selector] ensure: [ breakpoint remove ].