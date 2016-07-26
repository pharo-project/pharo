adding
add: newObject beforeIndex: index 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just before index. Answer newObject."
	(index between: 1 and: self size+1) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index - 1.
	^ newObject