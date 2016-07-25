new selection
computeSelectionIntervalForCurrentLine
	| i left right s quoteCounter|
	s := self string.
	i := s encompassParagraph: self selectionInterval.
	left := i first.
	right := i last.
	"we need to skip all text that is inside of quote pairs"
	quoteCounter := 0.
	[ left <= right and: [ (s at: left) = $"
			or: [ quoteCounter % 2 = 1
				or: [(s at: left) isSeparator] ] ] ]
		whileTrue: [ 
			(s at: left) = $" ifTrue: [ quoteCounter := quoteCounter + 1 ].
			left := left + 1 ].
	quoteCounter := 0.
	[ left <= right and: [ ((s at: right) = $"
			"we need to stop if quote is defined as character"
			and: [ right - 1 < 1 or: [ (s at: right - 1) ~= $$ ] ])
				or: [ quoteCounter % 2 = 1 or: [(s at: right) isSeparator] ] ] ]
		whileTrue: [
			(s at: right) = $" ifTrue: [ quoteCounter := quoteCounter + 1 ].
			right := right - 1 ].
	^ left to: right