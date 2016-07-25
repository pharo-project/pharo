accessing
resetCurrent
	"I reset all provider contents.
	 I do not free current because I do not want an update (see #current) each time I reset."
	current ifNil: [ ^ self ].
	self current initialize. "Reexecute initialization is dirty, but it makes the trick"