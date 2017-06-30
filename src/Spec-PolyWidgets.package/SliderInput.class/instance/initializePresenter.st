initialization
initializePresenter
	slider
		whenValueChangedDo: [ :sliderValue | 
			| inputValue |
			inputValue := input text asNumber. " ifNotNil: [ :text | text asNumber ]"
			sliderValue == inputValue
				ifFalse: [ input text: sliderValue asString ] ].
	input
		whenTextChanged: [ :text | 
			| inputValue |
			inputValue := text asNumber.
			slider value == inputValue
				ifFalse: [ slider value: inputValue ] ]