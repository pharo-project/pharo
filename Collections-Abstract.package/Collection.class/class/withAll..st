instance creation
withAll: aCollection
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size)
		addAll: aCollection;
		yourself