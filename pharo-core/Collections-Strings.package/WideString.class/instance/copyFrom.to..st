converting
copyFrom: start to: stop

	| n |
	n := super copyFrom: start to: stop.
	n isOctetString ifTrue: [^ n asOctetString].
	^ n.
