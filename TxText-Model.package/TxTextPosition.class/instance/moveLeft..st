navigation
moveLeft: number
	| newSpan newSpanPos |
	
	(number < 0)
		ifTrue: [ ^ self moveRight: number negated ].
		
	"Decrement spanPosition, if we're going to stay in same span,
	otherwise move prior to last character of previous span"
	newSpan := span.
	newSpanPos := spanPosition.
	
	number timesRepeat: [
		newSpanPos > 0 
			ifTrue: [ newSpanPos := newSpanPos - 1 ]
			ifFalse: [ 
				newSpan := newSpan prevIfNone: [  "can't go further than start " ^ self text startPosition ].
				newSpanPos := newSpan size - 1 max: 0.
			].
	].

	^ self class span: newSpan spanPosition: newSpanPos.
	