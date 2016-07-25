enumerating
do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]