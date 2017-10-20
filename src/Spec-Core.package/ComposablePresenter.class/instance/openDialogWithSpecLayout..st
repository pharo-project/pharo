api
openDialogWithSpecLayout: aSpec
	"Build the widget using the spec name provided as argument and display it into a window"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpec: aSpec ]
		ifFalse: [
			window value: (DialogWindowPresenter new model: self).
			window value openWithSpecLayout: aSpec.
			self initializeDialogWindow: window value.
			window value updateTitle.
			self takeKeyboardFocus ].
		
	^ window value