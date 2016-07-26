enumerating
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| aLink newCollection |
	newCollection := self class new.
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[newCollection add: (aBlock value: aLink value).
		 aLink := aLink nextLink].
	^ newCollection