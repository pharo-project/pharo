accessing
expectedDefects
	^ (errors, failures asOrderedCollection) select: [:each | each shouldPass not] 