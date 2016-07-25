requirements
element
	^ link ifNil: [link := ValueLink value: 42. "so that we can recognize this link"]