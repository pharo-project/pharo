window menu
openMenuAction
	"Open content of the file choosed by user in this workspace and associate it's name with this workspace"
	| userWarned |

	userWarned := true.
	(self fileName isNil and: [self contents isEmpty not or: [self hasUnacceptedEdits]]) ifTrue: [userWarned := self warnUser].
	(self fileName isNil not and: [self hasUnacceptedEdits]) ifTrue: [userWarned := self warnUser].
	
	userWarned ifTrue: [self openFile: self askForFileNameToOpen]