memory space
memoryHogs
	"Answer the list of objects to notify with #freeSomeSpace if memory gets full."

	^ MemoryHogs ifNil: [MemoryHogs := OrderedCollection new]