event handling
handleWindowEvent: anEvent
	"Handle an event concerning our host window"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(self wantsWindowEvent: anEvent) ifFalse:[^self].
	anEvent wasHandled: true.
	self windowEvent: anEvent.
