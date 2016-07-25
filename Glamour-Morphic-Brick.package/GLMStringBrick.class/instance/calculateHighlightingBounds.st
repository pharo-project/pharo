accessing
calculateHighlightingBounds
	|textBounds highlightBounds last|
	
	highlightBounds := OrderedCollection new.
	
	textBounds := (self calculateTextPosition extent: self textSize) translateBy: (0@0)negated.
	
	last := self text asLowercase findString: self highlightedText asLowercase startingAt: 1.
	last = 0 ifTrue: [^ highlightBounds].
	[last > 0] whileTrue: [ |beforeHighlightTextSize highlightSize matchingString|		
		matchingString := self text copyFrom: last to: last + self highlightedText size - 1.
		highlightSize := self sizeOf: matchingString.
		beforeHighlightTextSize := self sizeOf: (self text first: (last - 1)).
		highlightBounds add: ((textBounds translateBy: ((beforeHighlightTextSize x)@0)) withWidth: highlightSize x).
		last := self text asLowercase findString: self highlightedText asLowercase startingAt: last + 1 ].
	
	^ highlightBounds