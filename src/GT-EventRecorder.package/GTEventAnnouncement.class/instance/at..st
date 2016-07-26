accessing
at: key
	^ self dictionary at: key ifAbsent: [ nil ]