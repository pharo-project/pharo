initialization
initializePresenter

	workingCopies whenSelectedItemChanged: 
			[ :item | repositories items: (self repositoriesOfWorkingCopy: item) ].