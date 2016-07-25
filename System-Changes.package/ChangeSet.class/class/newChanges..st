current changeset
newChanges: aChangeSet 
	"Set the system ChangeSet to be the argument, aChangeSet."
	"pay attention not to use newChanges in current and other methods to avoid infinite cycles."
	
	self newChanges: aChangeSet withOld: self current.
	

	