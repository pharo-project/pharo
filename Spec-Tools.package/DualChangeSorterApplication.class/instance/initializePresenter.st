initialization
initializePresenter

	changeSorterLeft 	whenChangesListChanges: [ self refeshedChangeSet: changeSorterRight ].
	changeSorterRight	whenChangesListChanges: [ self refeshedChangeSet: changeSorterLeft ].

