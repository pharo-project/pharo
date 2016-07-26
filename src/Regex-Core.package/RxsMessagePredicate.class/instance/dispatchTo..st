accessing
dispatchTo: aBuilder
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aBuilder syntaxMessagePredicate: self