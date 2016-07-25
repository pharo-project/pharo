tests - equality
testHasEqualElements
	"self debug: #testHasEqualElements"

	self deny: (self empty hasEqualElements: self nonEmpty).
	self deny: (self nonEmpty hasEqualElements: self nonEmpty asSet). 
	self deny: (self nonEmpty reversed hasEqualElements: self nonEmpty).
	self deny: (self nonEmpty hasEqualElements: self nonEmpty reversed).