testing
hasValidBuildNumber
	"Return true if we have a valid build number"
	
	 ^self build ~= self class invalidBuildNumber