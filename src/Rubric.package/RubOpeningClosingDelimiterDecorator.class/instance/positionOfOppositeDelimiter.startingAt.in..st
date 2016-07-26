drawing
positionOfOppositeDelimiter: aDelimiter startingAt: aPosition in: aRectangle
	| string topLineIndex bottomLineIndex topLineFirst bottomLinelast oppositeDelimiter level here hereChar incr |
	string := self string.
	topLineIndex := self lineIndexForPoint: aRectangle topLeft.
	bottomLineIndex := self lineIndexForPoint: aRectangle bottomRight.
	topLineFirst := (self lines at: topLineIndex) first.
	bottomLinelast := (self lines at: bottomLineIndex) last.
	oppositeDelimiter := self openingDelimiters , self closingDelimiters
		at: (self closingDelimiters , self openingDelimiters indexOf: aDelimiter).
	(self openingDelimiters includes: aDelimiter)
		ifTrue: [ incr := [ :v | v + 1 ] ]
		ifFalse: [ incr := [ :v | v - 1 ] ].
	level := 1.
	here := aPosition - 1.
	[ level > 0 and: [ here > (topLineFirst max: 1) and: [ here <= bottomLinelast ] ] ]
		whileTrue: [ 
			here := incr value: here.
			here > string size
				ifTrue: [ ^ 0 ].
			hereChar := string at: here.
			hereChar = oppositeDelimiter
				ifTrue: [ 
					level := level - 1.
					level = 0
						ifTrue: [ ^ here ] ]
				ifFalse: [ 
					hereChar = aDelimiter
						ifTrue: [ level := level + 1 ] ] ].
	^ 0