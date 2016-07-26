instance creation
fromPointer: aHandle type: aType 
	"Instantiating a pointer as an array is usefull, but then you do not have the size. 
	 USE THIS WITH EXTREMELY CAUTION: You could have an overflow very easily. 
	 I would prefer if you use #fromPointer:type:size: instead. "
	^ self fromPointer: aHandle type: aType size: nil