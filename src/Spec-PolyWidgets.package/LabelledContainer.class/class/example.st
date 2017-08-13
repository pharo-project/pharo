example
example
	| example |
	example := self abstractExample.
	example content: ButtonModel.
	example subwidget
		label: 'I am a button'.
	example openDialogWithSpec.
	^ example