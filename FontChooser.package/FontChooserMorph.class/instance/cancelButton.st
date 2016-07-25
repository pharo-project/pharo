ui elements
cancelButton
	^cancelButton ifNil: [
		cancelButton := (self
			newButtonFor: self
			action: #cancelButtonClicked
			label: 'Cancel' translated
			help: 'Click here to cancel and close this dialog' translated)
			hResizing: #spaceFill ]