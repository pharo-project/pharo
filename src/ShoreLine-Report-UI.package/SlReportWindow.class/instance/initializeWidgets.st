initialization
initializeWidgets

	sendButton := self newButton.
	cancel := self newButton.
	autoSubmit := self newCheckBox.
	help := self newImage.
	stack := self newText beForText; disable; yourself.

	self initializeControls.