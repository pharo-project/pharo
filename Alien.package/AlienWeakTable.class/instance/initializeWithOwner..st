initialize-release
initializeWithOwner: anObject
	accessProtect := Semaphore forMutualExclusion.
	firstUnusedIndex := 1.
	lastUsedIndex := 0.
	weakArray := WeakArray new: 1024.
	strongArray := Array new: 1024.
	owner := anObject.
	WeakArray addWeakDependent: self