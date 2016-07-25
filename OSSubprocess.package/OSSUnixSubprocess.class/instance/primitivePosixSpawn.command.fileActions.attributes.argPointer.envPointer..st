private - primitives
primitivePosixSpawn: pidPointer command: commandString fileActions: fileActions attributes: attributes argPointer: argPointer envPointer: envPointer

	 ^ self ffiCall: #( int posix_spawnp (void* pidPointer, String commandString, void* fileActions, void * attributes, void* argPointer, void* envPointer) ) 
				