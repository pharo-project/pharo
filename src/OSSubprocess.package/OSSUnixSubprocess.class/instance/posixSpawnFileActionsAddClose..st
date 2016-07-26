close and dup2
posixSpawnFileActionsAddClose: aFileDescriptor
	"Add a close() action for aFileDescriptor for the child process"
	| returnValue |
	returnValue := self primitivePosixSpawnFileActionsAddClose: posixSpawnFileActionsT fileDescription: aFileDescriptor.
	returnValue = 0 ifFalse: [ self systemAccessor strerror: returnValue message: 'posix_spawn_file_actions_adddclose()' ].
	^ returnValue

