file system
directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	| path |
      "remove trailing slash - required for Windows compat"
      path := adirectoryPath last = $/
		ifTrue: [adirectoryPath allButLast] ifFalse: [adirectoryPath].
	^ anotherDirectoryPath directoryNamed: path