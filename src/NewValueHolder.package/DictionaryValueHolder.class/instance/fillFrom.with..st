protocol
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	aCollection keysAndValuesDo: [ :key :aValue |
		self at: key put: (aBlock value: aValue) ]