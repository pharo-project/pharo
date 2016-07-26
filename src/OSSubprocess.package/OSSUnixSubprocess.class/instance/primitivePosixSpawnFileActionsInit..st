private - primitives
primitivePosixSpawnFileActionsInit: aPosixSpawnFileActionsT 
	
	^ self ffiCall: #( int posix_spawn_file_actions_init(void* aPosixSpawnFileActionsT) )
	
