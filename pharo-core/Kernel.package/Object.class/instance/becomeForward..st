reflective operations
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	(Array with: self)
		elementsForwardIdentityTo:
			(Array with: otherObject)