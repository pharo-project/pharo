actions
updateDirectories
	"Update the directory tree and reselect the current."

	|dir|
	dir := self selectedFileDirectory.
	self changed: #directories.
	self selectDirectory: dir.
	self updateFiles