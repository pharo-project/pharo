listeners
addEventListener: anObject
	"Make anObject a listener for all events. All events will be reported to the object."
	self eventListeners: (self addListener: anObject to: self eventListeners)