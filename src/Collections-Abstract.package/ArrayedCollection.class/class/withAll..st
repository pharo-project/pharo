instance creation
withAll: aCollection
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size) replaceFrom: 1 to: aCollection size with: aCollection