typing support
setEmphasisHereFromTextForward: lookForward

	| i t forward delta prevIsSeparator nextIsSeparator |
	i := self pointIndex.
	t := self text.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsSeparator :=  i > 1 and: [ (t at: i-1) isSeparator ].
	nextIsSeparator := i <= t size and: [ (t at: i) isSeparator ].
	forward := prevIsSeparator = nextIsSeparator
		ifTrue: [ lookForward ]
		ifFalse: [ nextIsSeparator ].
	delta := forward ifTrue: [ 1 ] ifFalse: [ 0 ].
	self editingState emphasisHere: ((t attributesAt: (i - delta max: 1)) select: [:att | att mayBeExtended]).
	