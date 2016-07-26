enumerating
count: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Answer the number of elements that answered true."

	| sum |
	sum := 0.
	self do: [:each | (aBlock value: each) ifTrue: [sum := sum + 1]].
	^ sum