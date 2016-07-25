as yet unclassified
testIsLiberated3
	"By default it should answer yes."
	registry object: self.
	self deny: registry isLiberated.
	registry object: nil.
	self assert: registry isLiberated.