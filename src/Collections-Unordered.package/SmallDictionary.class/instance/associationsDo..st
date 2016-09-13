enumerating
associationsDo: aBlock 
	"Evaluate aBlock for each association for the receiver.

	Modifying the associations won't affect the receiver because it
	isn't implemented with associations like Dictionary."
	
	self keysAndValuesDo: [:key :value | aBlock value: key -> value]