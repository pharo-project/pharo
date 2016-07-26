fmp
deltaToEnsureInOrCentered: r extra: aNumber 
	| dX dY halfXDiff halfYDiff |
	dX := dY := 0.
	halfXDiff := ((r width - self width) * aNumber) truncated.
	halfYDiff := ((r height - self height) * aNumber) truncated.
	self left < r left 
		ifTrue: [ dX := self left - r left - halfXDiff ]
		ifFalse: 
			[ self right > r right ifTrue: [ dX := self right - r right + halfXDiff ] ].
	self top < r top 
		ifTrue: [ dY := self top - r top - halfYDiff ]
		ifFalse: 
			[ self bottom > r bottom ifTrue: [ dY := self bottom - r bottom + halfYDiff ] ].
	^ dX @ dY