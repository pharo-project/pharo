initialization
adapt: aComposableModel

	super adapt: aComposableModel.
	widget
		on: #click
		send: #click
		to: self