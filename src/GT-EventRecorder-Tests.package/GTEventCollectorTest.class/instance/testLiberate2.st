tests
testLiberate2
	collector occupant: self.
	self deny: collector isLiberated.
	collector liberate.
	self assert: collector isLiberated