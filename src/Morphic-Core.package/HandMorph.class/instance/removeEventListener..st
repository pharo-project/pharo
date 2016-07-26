listeners
removeEventListener: anObject
	"Remove anObject from the current event listeners."
	self eventListeners: (self removeListener: anObject from: self eventListeners).