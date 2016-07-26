testing
isAtLineEnd
	^ (self spanPosition = 0 and: [ self span isLineSeparator ])
		or: [  (self isAtSpanEnd and: [self span next isLineSeparator ])]