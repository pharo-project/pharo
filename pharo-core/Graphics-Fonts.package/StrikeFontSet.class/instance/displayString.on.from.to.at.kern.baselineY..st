displaying
displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY
	"Draw the given string from startIndex to stopIndex 
	at aPoint on the (already prepared) BitBlt."
	
	"Look for an excuse to use the fast primitive"
 	(aString hasWideCharacterFrom: startIndex to: stopIndex) ifTrue:[
		^ self displayMultiString: aString 
				on: aBitBlt 
				from: startIndex 
				to: stopIndex 
				at: aPoint 
				kern: kernDelta 
				baselineY: baselineY].

	^ aBitBlt displayString: aString 
			from: startIndex 
			to: stopIndex 
			at: aPoint 
			strikeFont: self
			kern: kernDelta