enumerating
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := self species new: self size.
	array do: [:each | each ifNotNil: [newSet add: (aBlock value: each enclosedSetElement)]].
	^ newSet