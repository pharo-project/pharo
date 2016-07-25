*Morphic-Widgets-ColorPicker
changeColor
	
	| dialog |
	dialog := ColorSelectorDialogWindow new
		title: 'Choose color';
		selectedColor: self color.
	self openModal: dialog.
	dialog cancelled
		ifFalse: [self fillStyle: dialog selectedColor]
	
	