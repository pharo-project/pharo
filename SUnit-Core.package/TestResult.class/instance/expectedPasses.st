accessing
expectedPasses
	^ passed select: [:each | each shouldPass] 