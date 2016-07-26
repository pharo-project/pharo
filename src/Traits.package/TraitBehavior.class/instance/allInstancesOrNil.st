accessing instances and variables
allInstancesOrNil
	"Answer all instances of the receiver, or nil if the primitive
	 fails, which it may be due to being out of memory."
	self error: 'Traits does not have instances.'