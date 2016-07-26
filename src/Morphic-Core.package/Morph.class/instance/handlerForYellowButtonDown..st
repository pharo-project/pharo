event handling
handlerForYellowButtonDown: anEvent 
	"Return the (prospective) handler for a mouse down event with the yellow button pressed.
	The 	handler is temporarily installed and can be used for morphs further 
	down the hierarchy to negotiate whether the inner or the outer 
	morph should finally handle the event."

	(self hasYellowButtonMenu or: [ self handlesMouseDown: anEvent ])
		ifFalse: [ ^ nil].	"Not interested."

	anEvent handler
		ifNil: [^ self].	"Nobody else was interested"

	"Same priority but I am innermost."
	^ self mouseDownPriority >= anEvent handler mouseDownPriority
		ifFalse: [nil ]
		ifTrue: [self]