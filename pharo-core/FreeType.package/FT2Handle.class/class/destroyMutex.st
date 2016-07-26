private
destroyMutex
	^ DestroyMutex ifNil: [ DestroyMutex := Mutex new ]