pointers management
freePointers
	"Frees all registered pointers. This must be called just after process is launched whether it
	was successful or not to avoid memory leaks... Also, all users of FFI #allocate: kind of method
	should either explicitly send #free (inside a ensure:) or register the allocated pointer so that 
	it can be free here."
	self pointers do: [ :pointer | pointer free ].
	pointers := nil