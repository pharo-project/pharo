displaying
displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta from: fromFont baselineY: baselineY
	| destPoint |
	destPoint := self
				displayString: aString
				on: aBitBlt
				from: startIndex
				to: stopIndex
				at: aPoint
				kern: kernDelta
				baselineY: baselineY.
	^ Array with: stopIndex + 1 with: destPoint