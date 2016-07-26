menus behavior
toggleBreakOnceOnEntryIn: aMethod
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint ifFalse: [ Breakpoint new node: aMethod ast; once; install]