close and dup2
destroyPosixSpawnFileActionsT
	"This is part of the needed API of posix_spawn() family of functions and dismply destroys
	the struct initialized by #initPosixSpawnFileActionsT"
	posixSpawnFileActionsT ifNotNil: [ 
		| returnValue |
		returnValue := self primitivePosixSpawnFileActionsDestroy: posixSpawnFileActionsT. 
		returnValue = 0 ifFalse: [ self systemAccessor strerror: returnValue message: 'posix_spawn_file_actions_distroy()'. ].	
	].