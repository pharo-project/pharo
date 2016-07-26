instance creation
newVersion: versionName
	"Install a new SystemVersion as the current version. Only preserve the higest update recorded.
	This method is a candidate for being removed, I am not sure it ever makes sense to do this."
	
	| newVersion |
	newVersion := self fromShortVersionString: versionName.
	newVersion
		highestUpdate: self current highestUpdate.
	Current := newVersion
