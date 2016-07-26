events-processing
handleMouseOver: anEvent
	"System level event handling."
	
	super handleMouseOver: anEvent.
	(self handlesMouseOver: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseOver: anEvent]