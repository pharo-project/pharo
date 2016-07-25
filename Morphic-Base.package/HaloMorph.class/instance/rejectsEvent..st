events-processing
rejectsEvent: anEvent
	"Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it."
	(super rejectsEvent: anEvent) ifTrue:[^true].
	anEvent isDropEvent ifTrue:[^true]. "never attempt to drop on halos"
	^false