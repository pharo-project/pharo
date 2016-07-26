navigation
moveToLineStart
	| sp |
	self isAtLineStart ifTrue: [  ^ self ].
	sp := span.
	(span isLineSeparator and: [ spanPosition = 0 ]) ifTrue: [ sp := span prev ].
	sp reverseDo: [ :each | each isLineSeparator ifTrue: [ ^ each endPosition ] ].
	^ self text startPosition