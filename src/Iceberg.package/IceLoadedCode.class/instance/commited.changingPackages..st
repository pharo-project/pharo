events
commited: newCommit changingPackages: packages
	self repository addCommit: newCommit.

	packages do: [ :package | self versionLoaded: (newCommit versionFor: package) ].
	referenceCommit := newCommit.
	repository announcer announce: IceCommited new.
