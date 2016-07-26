accessing
current
	^ self history isEmpty
		ifFalse: [self history last] 