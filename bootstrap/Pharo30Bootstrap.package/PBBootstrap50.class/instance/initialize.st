initialize-release
initialize

	super initialize.
	"the default espell backend for the bootstrap"
	espellBackend := EPOzVMBackend new.
	self instantiateClassLoader.