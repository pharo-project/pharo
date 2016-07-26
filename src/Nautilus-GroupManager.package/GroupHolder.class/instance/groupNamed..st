protocol
groupNamed: aString
	^ self groups detect: [:each | each name = aString]