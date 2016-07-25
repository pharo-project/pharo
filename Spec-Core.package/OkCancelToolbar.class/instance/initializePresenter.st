initialization
initializePresenter

	super initializePresenter.
	cancelButton action: [ self performCancelAction ]