private
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]