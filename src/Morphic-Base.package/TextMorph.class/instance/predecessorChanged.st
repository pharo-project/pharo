private
predecessorChanged
	| newStart oldStart |
	(self hasProperty: #CreatingParagraph) ifTrue: [^self].
	newStart := predecessor isNil 
				ifTrue: [1]
				ifFalse: [predecessor lastCharacterIndex + 1].
	(self paragraph adjustedFirstCharacterIndex ~= newStart 
		or: [newStart >= text size]) 
			ifTrue: 
				[paragraph composeAllStartingAt: newStart.
				self fit]
			ifFalse: 
				["If the offset to end of text has not changed, just slide"

				oldStart := self firstCharacterIndex.
				self withSuccessorsDo: [:m | m adjustLineIndicesBy: newStart - oldStart]]