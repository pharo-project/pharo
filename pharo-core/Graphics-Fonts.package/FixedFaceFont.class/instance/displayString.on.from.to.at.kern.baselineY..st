displaying
displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY
	| size |
	size := stopIndex - startIndex + 1.
	^ self perform: displaySelector withArguments: (Array with: aDisplayContext with: size with: aPoint with: kernDelta with: baselineY).