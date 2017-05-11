menus behavior
toggleBreakpointIn: aMethod
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint
		ifTrue: [ 
				aMethod ast nodesWithLinks do:[:link | Breakpoint removeFrom: link ]]
		ifFalse: [ Breakpoint new node: aMethod ast; always; install].
		
	SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol