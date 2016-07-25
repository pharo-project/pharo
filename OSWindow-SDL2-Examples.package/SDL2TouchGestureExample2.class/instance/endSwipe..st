manage swipe
endSwipe: anEvent
	blackBackgroundPosition := blackBackgroundPosition abs
		> (window extent x / 2)
		ifTrue: [ 0 ]
		ifFalse: [ window extent x ]