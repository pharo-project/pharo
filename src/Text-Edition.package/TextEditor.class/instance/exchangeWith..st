private
exchangeWith: prior
	"If the prior selection is non-overlapping and legal, exchange the text of
	 it with the current selection and leave the currently selected text selected
	 in the location of the prior selection (or leave a caret after a non-caret if it was
	 exchanged with a caret).  If both selections are carets, flash & do nothing.
	 Don't affect the paste buffer.  Undoer: itself; Redoer: Undoer."

	| start stop before currSelection priorSelection delta redoArgs altInterval undoArgs |
	start := self startIndex.
	stop := self stopIndex - 1.
	(((prior first <= prior last) or: [ start <= stop]) "Something to exchange" and:
			[self isDisjointFrom: prior])
		ifTrue:
			[before := prior last < start.
			currSelection := self selection.
			priorSelection := self text copyFrom: prior first to: prior last.
			delta := before ifTrue: [0] ifFalse: [priorSelection size - currSelection size].
			self zapSelectionWith: priorSelection.
			redoArgs := { prior. start to: stop}.
						
			self selectFrom: prior first + delta to: prior last + delta.
			delta := before ifTrue: [stop - prior last] ifFalse: [start - prior first].
			self zapSelectionWith: currSelection.
			
			altInterval := prior first + delta to: prior last + delta.
			undoArgs := {altInterval. self startIndex to: self stopIndex - 1}.
			"self undoer: #exchangeWith: with: altInterval."
			prior first > prior last ifTrue: [self selectAt: self editingState previousInterval last + 1].
			self editingState
				redoArray: { morph. #undoRedoExchange:with:. redoArgs} 
				undoArray: {morph. #undoRedoExchange:with:. undoArgs}]
		ifFalse:
			[morph flash]