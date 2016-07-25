enumerating
do: aBlock 
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]