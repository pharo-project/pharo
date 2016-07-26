accessing
versionFromPackageName: aPackageName commit: aCommitID
	"Explanation for code using the ancestry: if, when storing a dependency along with the package (single commit), that dependency is exactly the same as a previous store, git considers that it hasn't changed and gitfiletree correctly answers that there is no version of the dependency for that commit. We then have to search through the ancestry of the whole repository to find an older commit for that dependency."

	| entry mcR packageDirectoryName entries |
	packageDirectoryName := aPackageName , self packageExtension.
	(cacheFileNames and: [ allFileNames notNil ])
		ifTrue: [ entries := allFileNames select: [ :each | each packageDirectory = packageDirectoryName ] ]
		ifFalse: [ entries := self getVersionsForPackageDirectoryName: packageDirectoryName ].
	entry := entries detect: [ :each | each commitID = aCommitID ] ifNone: [  ].
	entry
		ifNil: [ 
			| cid a |
			a := self loadAncestry.
			cid := (a at: aCommitID) first.
			[ (entry := entries detect: [ :each | each commitID = cid ] ifNone: [  ]) isNil ]
				whileTrue: [ cid := (a at: cid) first ] ].
	mcR := MCFileTreeGitStReader on: directory fileName: packageDirectoryName.
	mcR commitID: entry commitID.
	mcR info: entry info.
	mcR repository: self.
	^ mcR version