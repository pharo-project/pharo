actions
browseImplementors
	"Browse the method implementors."

	self systemNavigation
		browseAllImplementorsOf: (self selectedMessageName ifNil: [^self])