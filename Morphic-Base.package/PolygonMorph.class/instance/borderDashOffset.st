dashes
borderDashOffset
	borderDashSpec size < 4 ifTrue: [^0.0].
	^ (borderDashSpec fourth) asFloat