private
loadLatestPackage: aString fromHub: aDirectoryName
	"Loads a package from smalltalkhub repository"

	self loadLatestPackage: aString from: self rootURLString, aDirectoryName, '/main'
	