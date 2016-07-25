opening
openWithFullView
	"Create and schedule a full debugger with the given label. Do not
	terminate the current active process."
	self 
		openWithSpec;
		updateToolbar;
		updateCodeFromContext