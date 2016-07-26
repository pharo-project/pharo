dependents access
canDiscardEdits
	"Return true if this view either has no text changes or does not care."

	^ (hasUnacceptedEdits and: [askBeforeDiscardingEdits]) not
