testing
areasRemainingToFill: aRectangle

	^self isOpaque
		ifTrue: [aRectangle areasOutside: self bounds]
		ifFalse: [Array with: aRectangle]