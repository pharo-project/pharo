example
example
	| example |
	example := self abstractExample.
	example content: ButtonPresenter.
	example subwidget
		label: 'I am a button'.
	example openDialogWithSpec.
	^ example