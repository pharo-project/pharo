events-processing
handleKeyUp: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue:[^self].
	(self handlesKeyUp: anEvent) ifFalse:[^self].
	anEvent wasHandled: true.
	^self keyUp: anEvent