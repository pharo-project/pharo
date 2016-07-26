class initialization
startUp: inANewImageSession
	"Any thunks in the finalization registry at the time the image comes up in a new session MUST NOT be finalized and should  immediately be discarded. Their thunk pointers are no longer valid."
	inANewImageSession ifTrue:
		[AllocatedThunks removeAll.
		ExecutablePages := Set new]