removing
removeAll
	"remove all the elements from this collection.
	Keep same amount of storage"
	
	self setCollection: (self class arrayType new: array size)