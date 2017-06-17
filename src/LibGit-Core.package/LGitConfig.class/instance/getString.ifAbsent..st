accessing
getString: name ifAbsent: aBlock
	^ self getString: name ifPresent: #yourself ifAbsent: aBlock