tests - equality
testEqualSignForSequenceableCollections
	"self debug: #testEqualSign"

	self deny: (self nonEmpty = self nonEmpty asSet). 
	self deny: (self nonEmpty reversed = self nonEmpty).
	self deny: (self nonEmpty = self nonEmpty reversed).