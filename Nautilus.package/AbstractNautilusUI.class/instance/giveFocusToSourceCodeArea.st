focus
giveFocusToSourceCodeArea
	self sourceTextModel 
		reconfigureViewWith: [ :scrolledText | 
			scrolledText textArea takeKeyboardFocus ]