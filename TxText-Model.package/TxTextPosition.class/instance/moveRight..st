navigation
moveRight: number
	| newSpan newSpanPos |
	(number < 0)
		ifTrue: [ ^ self moveLeft: number negated ].
		
	newSpan := span.
	newSpanPos := spanPosition.
	
	number timesRepeat: [ 
 		newSpanPos < newSpan size 
			ifTrue: [ newSpanPos := newSpanPos  + 1 ]
			ifFalse: [ 
				newSpan := newSpan nextIfNone: [ "reached end" ^ self text endPosition ].
"				self assert: newSpan size > 0."
				newSpanPos := 1 min: newSpan size ]
 	].

	^ self class span: newSpan spanPosition: newSpanPos
	