listeners
addMouseListener: anObject
	"Make anObject a listener for mouse events. All mouse events will be reported to the object."
	self mouseListeners: (self addListener: anObject to: self mouseListeners)