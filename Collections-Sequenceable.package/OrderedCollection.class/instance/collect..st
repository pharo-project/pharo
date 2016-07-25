enumerating
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection