logical operations
xor: alternativeBlock
	"Nonevaluating conjunction. If the receiver is true, answer the opposite of the 
	the argument, alternativeBlock; otherwise answer the value of the alternativeBlock."
	"
	true xor: [ true ]
	>>> false
	"
	^ alternativeBlock value not