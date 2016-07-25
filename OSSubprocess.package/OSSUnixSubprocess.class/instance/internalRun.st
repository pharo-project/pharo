running
internalRun
	"This is the real method that spwans the subprocess in the OS via the posix_spawn() function. 
	It first must deal with all the file acctions for close() and dup() in the child.
	Then spawns the process and sets the pid. 
	Finally, it registers the process in the OSSVMProcess vmProcess (so that the childWatcher considers this child).
	After this method is sent, we have to be sure to free resources (via #cleanResources)
	"
	
	| pidtPointer returnValue   |
	pidtPointer := ExternalAddress allocate: self systemAccessor sizeOfPointer.
	self registerPointer: pidtPointer.
	self closeAndDup2FilesActions.
	workingDirectory 
		ifNil: [ returnValue := self primitivePosixSpawn: pidtPointer command: command fileActions: posixSpawnFileActionsT attributes: nil argPointer: self buildPrimitiveArgVArgument envPointer: self buildPrimitiveEnvArgument. ] 
		ifNotNil: [ 
			"This is a hack in order to support a #workingDirectory: kind of feature. 
			Please read the comment of lockCwdWithValue:during: "
			OSSVMProcess vmProcess
				lockCwdWithValue: workingDirectory 
				during: [ returnValue := self primitivePosixSpawn: pidtPointer command: command fileActions: posixSpawnFileActionsT attributes: nil argPointer: self buildPrimitiveArgVArgument envPointer: self buildPrimitiveEnvArgument. ]
		].
	(returnValue > 0) 
		ifTrue: [ self systemAccessor strerror: returnValue message: 'posix_spawn()'. ].
	pid := pidtPointer nbInt32AtOffset: 0.
	OSSVMProcess vmProcess registerChildProcess: self.
	