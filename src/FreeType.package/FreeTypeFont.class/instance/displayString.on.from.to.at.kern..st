displaying
displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta
	
	^self displayString: aString
		on: aDisplayContext
		from: startIndex
		to: stopIndex
		at: aPoint
		kern: kernDelta
		baselineY: aPoint y  + self ascent