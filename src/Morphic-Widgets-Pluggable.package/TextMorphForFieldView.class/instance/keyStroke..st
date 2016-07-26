event handling
keyStroke: evt
	"Don't allow editing keys if the edit view is disabled.
	Do the key if max length has not been reached.
	Don't allow tabs." 
	(self localHandleKeystroke: evt) ifTrue: [^self].
	(self maxLength isNil or: [self text size < self maxLength or: [
			self editor selectionInterval size > 0 or: [self isEditEvent: evt]]])
		ifTrue: [self doAutoAccept: evt].
	super keyStroke: evt