step over
brotherPositionFor: nodePosition brothers: brothers
	^ nodePosition = brothers size ifTrue:  [ 1 ] ifFalse: [ nodePosition + 1 ].
