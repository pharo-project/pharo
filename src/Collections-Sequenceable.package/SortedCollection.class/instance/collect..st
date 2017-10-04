enumerating
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into an OrderedCollection. Answer the new collection.
	We cannot assume that the result is sorted, because aBlock can transform the 
	elements in arbitrary ways.  Thus, we must override the superclass in order
	to produce an OrderedCollection instead of a SortedCollection."

	| newCollection | 
	newCollection := OrderedCollection new: self size.
	self do: [:each | newCollection addLast: (aBlock value: each)].
	^ newCollection