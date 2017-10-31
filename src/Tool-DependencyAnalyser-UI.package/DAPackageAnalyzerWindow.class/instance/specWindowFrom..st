building
specWindowFrom: aSpec
	| windowPresenter specWindow |
	windowPresenter := aSpec window.
	specWindow := windowPresenter window.
	^ specWindow