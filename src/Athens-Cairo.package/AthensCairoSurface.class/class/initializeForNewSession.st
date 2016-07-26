session management
initializeForNewSession
	session := Smalltalk session.
	"create a dispatch structure"
	dispatchStruct := SQSurfaceDispatch externalNew.
	dispatchStruct 
		getSurfaceFormatFn: self createGetSurfaceFormatFn;
		lockSurfaceFn: self createLockSurfaceFn;
		unlockSurfaceFn: self createUnlockSurfaceFn;
		showSurfaceFn: self createShowSurfaceFn.
	"assign the pointer"
	dispatch := dispatchStruct getHandle.