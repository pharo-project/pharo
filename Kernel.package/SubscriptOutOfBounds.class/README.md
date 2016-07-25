I am SubscriptOutOfBounds, an exception indicating that some operation attempted to use a subscript outside allowed bounds.

Normally, I hold the offending subscript and/or the allowed lowerBound and upperBound (inclusive).

SubscriptOutOfBounds 
	signalFor: 10 
	lowerBound: 1 
	upperBound: 5 
	in: (Array new: 5)