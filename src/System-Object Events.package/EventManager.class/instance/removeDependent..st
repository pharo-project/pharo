dependents access
removeDependent: anObject
	"Remove the given object as one of the receiver's dependents."

	self 
		removeActionsWithReceiver: anObject
		forEvent: self changedEventSelector.
	^ anObject