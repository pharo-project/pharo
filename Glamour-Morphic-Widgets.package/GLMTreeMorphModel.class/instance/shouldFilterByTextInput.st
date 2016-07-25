search and filtering
shouldFilterByTextInput
	^ self glamourPresentation allowsFilter and: [ self inputText notEmpty ]