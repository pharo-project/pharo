class initialization
initialize
	"FFICallbackThunk initialize"
	AccessProtect := Semaphore forMutualExclusion.
	AllocatedThunks := AlienWeakTable newForOwner: self.
	ExecutablePages := Set new.
	self ensureNotifiedAtStartup