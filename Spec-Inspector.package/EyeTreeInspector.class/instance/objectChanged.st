event-handling
objectChanged
	self text doItReceiver: self object.
	self text widget
		ifNotNil: [ :w | w editingMode classOrMetaClass: self object class ].
	self tree
		roots: self roots;
		expandRoots