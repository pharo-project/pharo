menus behavior
toggleBreakOnEntryIn: aMethod
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint
		ifTrue: [ Breakpoint removeFrom: aMethod ast]
		ifFalse: [ Breakpoint new node: aMethod ast; always; install]