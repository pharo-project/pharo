instance creation
nilUUID
	"Return a empty un-initialized UUID, with all zero values, 00000000-0000-0000-0000-000000000000"
	
	"self nilUUID"
	
	"Must call basicNew: here because I have a non-trivial initialize method."

	^ self basicNew: 16