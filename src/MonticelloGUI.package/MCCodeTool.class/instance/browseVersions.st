menus
browseVersions
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	
	Smalltalk tools versionBrowser  
		browseVersionsForClass:  self selectedClassOrMetaClass 
		selector: self selectedMessageName
		