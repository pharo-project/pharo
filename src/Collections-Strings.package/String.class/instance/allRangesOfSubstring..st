accessing
allRangesOfSubstring: aSubstring

	^ Array streamContents: [:s | | start subSize |
		start := 1.
		subSize := aSubstring size.
		[start isZero]
			whileFalse: [ start := self findString: aSubstring startingAt: start.
				start > 0 
					ifTrue: [s nextPut: (start to: start + subSize - 1).
						start := start + subSize]]]