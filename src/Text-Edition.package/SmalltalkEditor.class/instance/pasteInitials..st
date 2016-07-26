editing keys
pasteInitials: aKeyboardEvent 
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."
	self closeTypeIn.
	self replace: self selectionInterval with: (Text fromString: Author changeStamp) and: [self selectAt: self stopIndex].
	^ true