accessing class hierarchy
withAllSubclassesDo: aBlock
	"Evaluate the argument, aBlock, for the receiver and each of its 
	subclasses."
	self withAllSubclasses do: [ :subclass | aBlock value: subclass ].