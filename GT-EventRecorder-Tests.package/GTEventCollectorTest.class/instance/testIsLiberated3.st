tests
testIsLiberated3
	"The occupant has to be empty."
	collector occupant: self.
	self deny: collector isLiberated.