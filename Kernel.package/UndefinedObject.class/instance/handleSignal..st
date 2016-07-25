bottom context
handleSignal: exception
	"When no more handler (on:do:) context left in sender chain this gets called.  Return from signal with default action."

	^ exception resumeUnchecked: exception defaultAction