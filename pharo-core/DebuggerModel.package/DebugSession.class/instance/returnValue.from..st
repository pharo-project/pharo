debugging actions
returnValue: anObject from: aContext
	"Force a return of a given value to the previous context!"

	| previous |
	self flag: 'should be called only on the selected context. WHY?'.	
	(self isContextPostMortem: aContext) ifTrue: [^ self].

	previous := aContext sender.
"	self resetContext: previous."
	interruptedProcess popTo: previous value: anObject.
	self updateContextTo: previous.
	self contextChanged 