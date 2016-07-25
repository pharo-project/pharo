testing
isEmpty
	^ (self hasScheme | self hasHost | self hasPath | self hasQuery | self hasFragment | self hasUsername | self hasPassword) not