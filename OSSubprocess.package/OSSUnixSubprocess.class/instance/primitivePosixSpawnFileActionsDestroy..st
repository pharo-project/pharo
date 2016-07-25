private - primitives
primitivePosixSpawnFileActionsDestroy: aPosixSpawnFileActionsT 
	
	^ self ffiCall: #( int posix_spawn_file_actions_destroy(void*  aPosixSpawnFileActionsT) )
	


