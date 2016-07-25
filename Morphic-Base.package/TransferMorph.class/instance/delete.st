submorphs-add/remove
delete
	"See also >>justDroppedInto:event:."
	accepted ifFalse: [
		self dropNotifyRecipient ifNotNil: [ self dropNotifyRecipient dropRejectedMorph: self ]].
	self changed: #deleted.
	self breakDependents.
	super delete