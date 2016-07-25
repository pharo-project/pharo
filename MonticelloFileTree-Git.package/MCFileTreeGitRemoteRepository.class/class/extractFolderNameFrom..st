instance creation
extractFolderNameFrom: aString
	"Return a folder name for that url."

	^ (aString substrings: '/') last copyUpToLast: $.