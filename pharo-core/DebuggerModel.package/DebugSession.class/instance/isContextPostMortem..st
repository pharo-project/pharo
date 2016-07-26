testing
isContextPostMortem: selectedContext
	"return whether we're inspecting a frozen exception without a process attached"
	| suspendedContext |
	suspendedContext := interruptedProcess suspendedContext.
	suspendedContext ifNil: [ ^ false ].
	(suspendedContext == selectedContext)
		ifTrue: [ ^ false ].
	^ (suspendedContext findContextSuchThat: [:c | c sender == selectedContext]) isNil