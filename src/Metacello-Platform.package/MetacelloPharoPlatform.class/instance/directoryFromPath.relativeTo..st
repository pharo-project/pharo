file system
directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	^ anotherDirectoryPath resolveString: adirectoryPath