initialization
updateDirectory
	"directory has been changed externally, by calling directory:.
	Now change the view to reflect the change."
	self changed: #directory.
	self changed: #initialDirectoryList.
	self postOpen.