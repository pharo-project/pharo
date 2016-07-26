instance creation
fromHandle: aHandle type: aType
	"Instantiating a handle as an array.
	 USE THIS WITH EXTREMELY CAUTION: You could have an overflow very easily, because you are 
	 not expliciting the size. 
	 I would prefer if you use #fromHandle:type:size: instead. "
	^ self fromHandle: aHandle type: aType size: nil