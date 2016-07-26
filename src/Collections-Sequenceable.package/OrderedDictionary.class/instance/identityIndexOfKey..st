accessing
identityIndexOfKey: aKey
	^ self identityIndexOfKey: aKey ifAbsent: [0]