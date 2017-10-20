initialization
adapt: aComposablePresenter

	super adapt: aComposablePresenter.
	widget
		on: #click
		send: #click
		to: self