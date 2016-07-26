private accessing
originReferences
	"Returns the set of ports (instances of PortReference) the transmission receives its data from"
	^ origins ifNil: [origins := OrderedCollection new]