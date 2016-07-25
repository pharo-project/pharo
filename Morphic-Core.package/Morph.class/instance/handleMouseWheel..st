events-processing
handleMouseWheel: anEvent
	"System level event handling."
	
	anEvent wasHandled ifTrue:[^self].
	(self handlesMouseWheel: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseWheel: anEvent]