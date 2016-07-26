enumerating
collect: aBlock into: aCollection
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into aCollection. Answer aCollection."

	^aCollection fillFrom: self with: aBlock