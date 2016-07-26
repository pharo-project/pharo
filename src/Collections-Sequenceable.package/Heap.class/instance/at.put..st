accessing
at: index put: newObject
	"Heap are not designed to be accessed sequentially.
	Please consider using #add: instead."
	
	self shouldNotImplement.