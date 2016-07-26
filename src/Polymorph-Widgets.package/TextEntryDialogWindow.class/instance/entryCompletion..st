accessing
entryCompletion: anEntryCompletion
	anEntryCompletion chooseBlock: [:t | self ok].
	self textEditorMorph entryCompletion: anEntryCompletion

