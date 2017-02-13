instance creation
nilUUID
	"Return a empty un-initialized UUID, with all zero values"
	
	"Must call basicNew: here because I have a non-trivial initialize method."

	^ self basicNew: 16