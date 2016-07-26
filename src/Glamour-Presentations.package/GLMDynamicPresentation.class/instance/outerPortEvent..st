events
outerPortEvent: aPortEvent
	
	super outerPortEvent: aPortEvent.
	"Propage the event to cached presentations"
	self cachedPresentation ifNotNil: [ 
		self cachedPresentation outerPortEvent: aPortEvent ] 