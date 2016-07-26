displaying
displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta from: fromFont 
	| destPoint |
	destPoint := self
				displayString: aString
				on: aBitBlt
				from: startIndex
				to: stopIndex
				at: aPoint
				kern: kernDelta.
	^ Array with: stopIndex + 1 with: destPoint