defaults
defaultChangeSetDirectory: dirOrName 
	"Set the Preference for storing change sets to the given directory or name (possibly relative).
	Rewrite directory names below the default directory as relative names.
	If dirOrName is an empty string, use the default directory."

	"ChangeSet defaultChangeSetDirectory: 'changeSets'"

	| dirName defaultFullName |
	
	dirName := dirOrName isString
				ifTrue: [ dirOrName asFileReference fullName ]
				ifFalse: [dirOrName fullName].
				
	defaultFullName := FileSystem workingDirectory fullName.
	dirName = defaultFullName
		ifTrue: [dirName := '']
		ifFalse: [
			(dirName beginsWith: defaultFullName , FileSystem disk delimiter asString)
				ifTrue: [dirName := dirName copyFrom: defaultFullName size + 2 to: dirName size]].
	self defaultChangeSetDirectoryName: dirName