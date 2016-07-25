accessing settings
cacheDirectory: directory
	"directory - can be an absolutePathString (to interface with the settings framework), or aFileReference"
 
	default := self new directory: directory asFileReference.