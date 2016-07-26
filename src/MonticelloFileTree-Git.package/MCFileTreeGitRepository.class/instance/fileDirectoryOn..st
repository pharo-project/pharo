private
fileDirectoryOn: aGitFileTreePackageEntry
	"We are dealing with GitFileTreePackageEntry objects, not FileReferences."

	^ self fileUtils directoryFromPath: aGitFileTreePackageEntry packageDirectory relativeTo: self directory