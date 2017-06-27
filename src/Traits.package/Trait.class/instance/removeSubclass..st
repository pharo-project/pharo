accessing class hierarchy
removeSubclass: aSubclass 
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."

	self subclasses == nil ifFalse:
		[self subclasses:  (self subclasses copyWithout: aSubclass).
		self subclasses isEmpty ifTrue: [self subclasses: nil]].