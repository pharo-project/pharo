step over
brotherPositionFor: nodePosition brothers: brothers
	^ nodePosition = 1 ifTrue:  [ brothers size ] ifFalse: [ nodePosition - 1 ].