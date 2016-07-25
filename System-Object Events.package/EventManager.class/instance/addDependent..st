dependents access
addDependent: anObject
	"Make the given object one of the receiver's dependents."

	self
		when: self changedEventSelector
		send: self updateEventSelector
		to: anObject.
	^anObject