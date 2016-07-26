testing
appliesToDebugger: aDebugger
	| interruptedContext |
	
	interruptedContext := aDebugger session interruptedContext.
	^ (interruptedContext selector == #subclassResponsibility) and: [ 
		"If we work with an instance of the abstract class (shouldn't normally happen) do not add this action, 
		as the abstract class alredy contains the the method."
		interruptedContext sender receiver class ~~ interruptedContext sender methodClass  ]

