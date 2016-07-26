class initialization
initialize
	"
	self initialize
	"
	"ensure that other classes have also been initialized by forcefully initializing them now.
	It then does not matter which order they are initialized in during the package load"
	FT2Constants initialize.
	FreeTypeCache initialize.
	FreeTypeCacheConstants initialize.
	FreeTypeSettings initialize.
	
	current := nil.
	self current. "this creates an instance of me, and updates from the system"