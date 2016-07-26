development support
goferCommitBranchPackage: branchName using: repositorySpecs commitMessage: commitMessage
	| gofer repoSpecs wc |
	repoSpecs := self spec repositorySpecs notEmpty
		ifTrue: [ self spec repositorySpecs ]
		ifFalse: [ repositorySpecs ].
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	wc := self spec workingCopy.
	repositorySpecs
		do: [ :repoSpec | 
			| repo |
			repo := repoSpec createRepository.
			(wc possiblyNewerVersionsIn: repo) notEmpty
				ifTrue: [ 
					self
						notify:
							'There are possibly newer versions of the package ' , self spec name printString , ' in the repository '
								, repo description printString
								, '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
			gofer repository: repo ].
	gofer package: self spec name.
	[ gofer interactiveCommit ]
		on: MCVersionNameAndMessageRequest
		do: [ :ex | 
			| ref |
			ref := GoferVersionReference name: ex suggestedName.
			ex
				resume:
					{(ref packageName , '.' , branchName , '-' , ref author , '.' , ref versionNumber printString).
					commitMessage} ].
	^ true