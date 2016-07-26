enumerating
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (aBlock value: (self at: index))].
	^ newCollection