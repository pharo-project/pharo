tests
testIsLiberated2
	"The collection of events has to be empty."
	collector add: 42.
	self deny: collector isLiberated.