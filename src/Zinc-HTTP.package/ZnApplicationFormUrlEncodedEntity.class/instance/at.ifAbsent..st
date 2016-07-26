accessing
at: key ifAbsent: block
	^ self fields at: key ifAbsent: block