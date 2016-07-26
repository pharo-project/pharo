enumeration
specListDetect: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true."

	^ self specListDetect: aBlock ifNone: [ self error: 'Object is not in the collection.' ]