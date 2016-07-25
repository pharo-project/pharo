*Tool-FileList
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	"Answer services for the given file"

	^ ((self isSourceFileSuffix: suffix) or: [ suffix = '*' ])
		ifTrue:
			[{self serviceRemoveLineFeeds.
			self serviceFileIn}]
		ifFalse:
			[#()]