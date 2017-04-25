menus behavior
toggleBreakpointIn: aMethod
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint
		ifTrue: [ Breakpoint removeFrom: aMethod ast]
		ifFalse: [ Breakpoint new node: aMethod ast; always; install].
		
	SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol