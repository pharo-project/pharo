instance creation
new: size withAll: value 
	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^(self new: size) atAllPut: value