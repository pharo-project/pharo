querying
defaultPackageNamesToLoad: defaultList
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	^ self packageAndProjectNamesToLoad: defaultList loader: self loader