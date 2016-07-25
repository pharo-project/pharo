listeners
removeMouseListener: anObject
	"Remove anObject from the current mouse listeners."
	self mouseListeners: (self removeListener: anObject from: self mouseListeners).