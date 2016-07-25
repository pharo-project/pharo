navigation
moveDown: number
	"Answer the new position, *number* of lines below than receiver's line,
	trying to preserve the same line position"

	| linePos newSpan |
	
	(number < 0)
		ifTrue: [ ^ self moveUp: number negated ].
		
	linePos := self linePosition.
	newSpan := span.
	number timesRepeat: [
		newSpan := newSpan nextLineSeparatorIfNone: [ ^ self moveToEnd ].
	].
	
	^ newSpan endPosition linePosition: linePos.
	