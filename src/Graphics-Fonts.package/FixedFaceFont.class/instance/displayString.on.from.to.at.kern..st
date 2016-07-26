displaying
displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta 
	| size |
	size := stopIndex - startIndex + 1.
	^ self perform: displaySelector withArguments: (Array with: aDisplayContext with: size with: aPoint with: kernDelta with: aPoint y + self ascent).