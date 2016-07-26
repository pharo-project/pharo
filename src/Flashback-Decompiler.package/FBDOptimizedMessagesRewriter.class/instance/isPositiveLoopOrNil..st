testing
isPositiveLoopOrNil: rcvr
	rcvr selector = #<=
		ifTrue: [ ^ true ].
	rcvr selector = #>=
		ifTrue: [ ^ false ].
	^ nil