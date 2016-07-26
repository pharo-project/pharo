*System-Changes
removeFromChanges
	"References to the receiver, a class, and its metaclass should no longer be included in the system ChangeSet."

	ChangeSet current removeClassAndMetaClassChanges: self