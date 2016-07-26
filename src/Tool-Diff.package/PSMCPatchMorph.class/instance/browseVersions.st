actions
browseVersions
	"Browse the method versions."

	Smalltalk tools versionBrowser 
			browseVersionsForClass: self selectedChangeWrapper actualClass 
			selector: self selectedMessageName
			