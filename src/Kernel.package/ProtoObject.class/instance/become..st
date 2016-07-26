reflective operations
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	(Array with: self)
		elementsExchangeIdentityWith:
			(Array with: otherObject)