close and dup2
closeAndDup2FilesActions
	"First, it initializes the structure for managing the file actions allowed by posix_spwan(). 
	Then defines and sets all actions we want. That is, closing or opening file descriptors,
	or doing a dup2() "
	self initPosixSpawnFileActionsT.
	self closeAllButStandardFileStreams.
	self defineActionsForStandardFileStreams.
	