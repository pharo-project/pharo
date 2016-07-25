ui requests
merge: merger informing: aString
	"Check for conflicts, if there are none, just continue. 
	 Merger will accept all conflicts"
	super merge: merger informing: aString.
	merger hasConflicts ifTrue: [ self exitFailure ]