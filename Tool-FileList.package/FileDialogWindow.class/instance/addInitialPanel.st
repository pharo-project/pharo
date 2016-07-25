actions
addInitialPanel
	"Add the panel."

	super addInitialPanel.
	self selectDirectory: FileSystem disk workingDirectory 