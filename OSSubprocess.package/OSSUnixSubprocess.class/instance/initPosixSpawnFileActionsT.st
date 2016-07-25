close and dup2
initPosixSpawnFileActionsT
	"This is part of the needed API of posix_spawn() family of functions and initializes
	the struct posix_spawn_file_actions_t"
	| returnValue |
	self allocatePosixSpawnFilesActionStructure.
	self registerPointer: posixSpawnFileActionsT.
	returnValue := self primitivePosixSpawnFileActionsInit: posixSpawnFileActionsT. 
	returnValue = 0 ifFalse: [ self systemAccessor strerror: returnValue message: 'posix_spawn_file_actions_init()' ].