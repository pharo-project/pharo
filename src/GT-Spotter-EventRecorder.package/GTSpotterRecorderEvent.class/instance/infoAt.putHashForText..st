accessing
infoAt: key putHashForText: valueString 
	^ self 
		infoAt: key 
		put: (self hashForText: valueString)