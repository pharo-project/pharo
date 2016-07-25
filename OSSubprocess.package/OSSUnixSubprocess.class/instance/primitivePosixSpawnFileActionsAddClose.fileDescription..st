private - primitives
primitivePosixSpawnFileActionsAddClose: fileActions fileDescription: fileDescriptionNumber
	
	^ self ffiCall: #( int posix_spawn_file_actions_addclose ( void* fileActions, int fileDescriptionNumber) )