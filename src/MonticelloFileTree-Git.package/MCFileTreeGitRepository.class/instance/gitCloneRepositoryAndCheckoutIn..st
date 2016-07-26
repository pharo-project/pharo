git querying
gitCloneRepositoryAndCheckoutIn: aDirectoryName
	"Do a clone on the remote repository and do a checkout on it to get the right branch. Complement the directory as well with the working directory. Note the name change when the directory is read-only: this is to protect of a Metacello problem when loading from a configuration with both a read-only and a read-write target (do I also need to protect when dealing with branches?)."

	"Metacello issues: even when selecting a specific version, will I load all repositories? It seems no."

	"Interesting. I can't use relative paths with MCFileTreeUtils"

	| targetDir targetDirName |
	targetDirName := readonly
		ifTrue: [ aDirectoryName , '-readonly' ]
		ifFalse: [ aDirectoryName ].
	targetDir := targetDirName asFileReference.
	targetDir exists
		ifTrue: [ 
			| targetDirBranch |
			self
				directory:
					(subdirectory isEmpty
						ifTrue: [ targetDir ]
						ifFalse: [ targetDir / subdirectory ]).
			targetDirBranch := self currentBranch.
			((branch isNil and: [ targetDirBranch = 'master' ]) or: [ targetDirBranch = branch ])
				ifFalse: [ 
					self
						error:
							'GitFileTree target directory already exists and is on another branch, cancelling clone and repository instanciation : '
								, (self fileUtils directoryPathString: targetDir) ] ]
		ifFalse: [ 
			self
				gitCommand:
					#('clone')
						,
							(branch
								ifNil: [ #() ]
								ifNotNil: [ 
									{'-b'.
									branch} ])
						,
							(readonly
								ifTrue: [ #('--depth' '1') ]
								ifFalse: [ #() ])
						,
							{remoteUrl.
							targetDirName}
				in: FileSystem workingDirectory.
			GrowlMorph openWithLabel: 'gitfiletree://' contents: 'Cloning remote repository: done!'.
			self
				directory:
					(subdirectory isEmpty
						ifTrue: [ targetDir ]
						ifFalse: [ targetDir / subdirectory ]) ]