private
depsSatisfying: selectBlock versionDo: verBlock displayingProgress: progressString
	| repoMap count |
	repoMap := Dictionary new.
	self repositories do: [:repo |
		MCRepositoryGroup default addRepository: repo.
		repo allVersionNames
			ifEmpty: [self logWarning: 'cannot read from ', repo description]
			ifNotEmpty: [:all | all do: [:ver | repoMap at: ver put: repo]]].

	count := 0.
	self dependencies do: [:dep |
		| ver repo |
		ver := dep versionInfo name.
		repo := repoMap 
					at: ver 
					ifAbsent: 
						[self logError: 'Version ', ver, ' not found in any repository'.
						self logError: 'Aborting'.
						^count].
		(selectBlock value: dep) 
			ifTrue: [| new |
					new := self versionNamed: ver for: dep from: repo.
					new ifNil: 
							[self logError: 'Could not download version ', ver, ' from ', repo description.
							self logError: 'Aborting'.
							^count]
						ifNotNil: 
							[ self logUpdate: dep package with: new.
							ProgressNotification signal: '' extra: 'Installing ', ver.
							verBlock value: new.
							count := count + 1]].
		dep package workingCopy repositoryGroup addRepository: repo.
	] displayingProgress: progressString.
	^count