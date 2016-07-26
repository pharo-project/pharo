development support
goferCommitPackageUsing: repositorySpecs commitMessage: commitMessage

	| gofer repoSpecs wc |
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	wc := self spec workingCopy.
	repositorySpecs do: [:repoSpec | | repo |
		repo := repoSpec createRepository.
		(wc possiblyNewerVersionsIn: repo) notEmpty
			ifTrue: [ self notify: 'There are possibly newer versions of the package ', self spec name printString, ' in the repository ', repo description printString, '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
		gofer repository: repo ].
	gofer package: self spec name.
	gofer commit: commitMessage.
	^true