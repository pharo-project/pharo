private
shouldInclude: aLGitCommit 
	^ self modifyingPackage 
		ifNil: [ true ]
		ifNotNil: [ :package | 
			aLGitCommit changesFileNamed: package directoryPathString ]
