*GT-SpotterExtensions-Core
spotterActDefault
	^ self isTopWindow
		ifTrue: [self comeToFront] "rise above non-window morphs"
		ifFalse:[	self activate].
