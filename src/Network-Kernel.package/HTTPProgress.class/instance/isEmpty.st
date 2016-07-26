testing
isEmpty
	"Answer true if I do not contain a numerical progress indication."
  	
	^ amount isNil or: [ total isNil ]