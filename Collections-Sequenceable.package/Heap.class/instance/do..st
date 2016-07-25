enumerating
do: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument."
	1 to: tally do:[:i| aBlock value: (array at: i)]