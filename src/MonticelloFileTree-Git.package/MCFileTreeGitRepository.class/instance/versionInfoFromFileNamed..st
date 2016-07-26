accessing
versionInfoFromFileNamed: aGitFileTreePackageEntry
	"Gofer/Monticello API: need to cover that."

	"I should not answer to that, since I don't deal with filenames."

	| mcR packageName |
	self deprecated: 'I do not answer to queries by filenames'.
	self assert: aGitFileTreePackageEntry class == GitFileTreePackageEntry.
	packageName := (self class parseName: aGitFileTreePackageEntry info name) first , self packageExtension.
	mcR := MCFileTreeGitStReader on: directory fileName: packageName.
	mcR commitID: aGitFileTreePackageEntry commitID.
	mcR info: aGitFileTreePackageEntry info.
	mcR repository: self.
	^ mcR version info