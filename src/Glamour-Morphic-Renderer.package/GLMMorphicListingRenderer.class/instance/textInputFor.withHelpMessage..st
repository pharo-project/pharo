private
textInputFor: aTreeModel withHelpMessage: helpStringMessage
	| textInput |
	textInput := self theme
		newTextEntryIn: nil
		for: aTreeModel
		get: #inputText
		set: #inputText:
		class: String
		getEnabled: #inputTextEnabled
		help: helpStringMessage.
	textInput askBeforeDiscardingEdits: false.
	textInput ghostText: helpStringMessage.
	textInput layoutFrame: (LayoutFrame identity 
			topFraction: 1;
			topOffset: -26;
			bottomOffset: -4).
	^ textInput