private
markPackagesBeforeNewCodeIsLoaded
	"Use this method to keep a log of all the packages that were loaded before loading new code. This will help the system to perform a diff and know after what to publish."
	
	"self new markPackagesBeforeNewCodeIsLoaded"
	
	PackagesBeforeLastLoad := self currentVersionsToBeSaved