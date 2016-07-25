accessing
currentNodelist
	^ self nodeList copyFrom: 1 to: (self lastIndex min: self nodeList size)