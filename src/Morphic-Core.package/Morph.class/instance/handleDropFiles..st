events-processing
handleDropFiles: anEvent
	"Handle a drop from the OS."
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(self wantsDropFiles: anEvent) ifFalse:[^self].
	anEvent wasHandled: true.
	self dropFiles: anEvent.
