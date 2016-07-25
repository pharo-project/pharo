accessing
indexOfKey: aKey
	^ self indexOfKey: aKey ifAbsent: [0]