enumerating
reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, 
	starting with the last element and taking each in sequence up to the 
	first. For SequenceableCollections, this is the reverse of the enumeration 
	for do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]