private
unplug
	self unplugFindReplace.
	self releaseEditingState.
	self releaseParagraph.
	self releaseEditor.
	super unplug
	