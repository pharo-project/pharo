new selection
selectLine
	"Make the receiver's selection, if it currently consists of an insertion point only, encompass the current line."
	| i left right s |
	self hasSelection ifTrue: [^self].
	s := self string.
	i := s encompassParagraph: self selectionInterval.
	left := i first.
	right := i last.
	[ left <= right and: [ (s at: left) = $" or: [(s at: left) isSeparator] ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ (s at: right) = $"  or: [(s at: right) isSeparator] ] ]
		whileTrue: [ right := right - 1 ].
	self selectInterval: (left to: right)