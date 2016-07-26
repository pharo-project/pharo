initialization
setView: newView

	view == newView ifTrue:  [ ^ self ].
	view delete.
	view := newView.
	self addMorph: view.

	decorators do: [ :each | each setupView: newView ]