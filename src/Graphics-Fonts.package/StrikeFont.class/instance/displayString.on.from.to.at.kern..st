displaying
displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta
	"Draw the given string from startIndex to stopIndex 
	at aPoint on the (already prepared) BitBlt."
	
		"Somewhat of a hack:
	The scanner seem to only pass in runs of either all ASCII or all non-ASCII characters. If all characters are ascii, then it's safe to use basic rendering-method."
	(aString hasWideCharacterFrom: startIndex to: stopIndex) ifTrue:  [^ self displayMultiString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: aPoint y + self ascent.].

	^ aBitBlt displayString: aString 
			from: startIndex 
			to: stopIndex 
			at: aPoint 
			strikeFont: self
			kern: kernDelta.