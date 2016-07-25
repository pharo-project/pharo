close and dup2
closeAllButStandardFileStreams
	"Here I should loop and close all but fd 0 1 2"

	"do a loop with #posixSpawnFileActionsAddClose:"
	
	self flag: #toDo