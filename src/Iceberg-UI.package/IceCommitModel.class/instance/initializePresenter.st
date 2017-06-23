initialization
initializePresenter
	commitButton action: [ self commitPackages ].
	commitPushButton action: [ self commitAndPushPackages ]