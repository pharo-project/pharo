error correction
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection |
	"If we can't ask the user, assume that the keyword will be defined later"
	(self interactive and: [ self warns ])
		ifFalse: [^proposedKeyword asSymbol].

	userSelection := requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.
	requestor select.

	correctSelector := UnknownSelectorDetected name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]