private - primitives
primitivePosixSpawnFileActionsAddDup2: fileActions oldFileDescription: oldFileDescription newFileDescritpion: newFileDescription 

	 ^ self ffiCall: #( int posix_spawn_file_actions_adddup2 ( void* fileActions, int oldFileDescription, int newFileDescription ) )