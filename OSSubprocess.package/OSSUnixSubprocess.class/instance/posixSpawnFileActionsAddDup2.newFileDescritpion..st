close and dup2
posixSpawnFileActionsAddDup2: oldFileDescription newFileDescritpion: newFileDescription
	"Add a dup2() action for from oldFileDescription to newFileDescription, for the child process"
	| returnValue |
	returnValue := self primitivePosixSpawnFileActionsAddDup2: posixSpawnFileActionsT oldFileDescription: oldFileDescription newFileDescritpion: newFileDescription.
	returnValue = 0 ifFalse: [ self systemAccessor strerror: returnValue message: 'posix_spawn_file_actions_adddup2()' ].
	^ returnValue

